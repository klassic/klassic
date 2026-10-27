/* Win64 API model for executing a complete generated PE on x86-64 POSIX.
 * Native Windows uses the actual kernel APIs through these same wrappers. */
#include <assert.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#define WINABI
#define ABI_CHECK() ((void)0)
#else
#include <pthread.h>
#include <sched.h>
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/stat.h>
#define WINABI __attribute__((ms_abi))
#define ABI_CHECK() assert(((uintptr_t)__builtin_frame_address(0)&15)==0)
#endif
static int fault;
static _Atomic unsigned tls_keys_active;
static _Atomic unsigned events,threads,waits,timeouts,signals,closes,main_waits,notified,clocks,decommits,yields,join_waits,joined,thread_closes,event_closes,worker_returns;
static uint64_t dirty(uint32_t n){return UINT64_C(0xa55a5aa500000000)|n;}
static _Thread_local uint32_t last_error;
static _Atomic uint64_t idle_clock_origin;
static _Atomic unsigned filesystem_calls,searches_open;
static uint64_t raw_counter(void);
static uint64_t counter_frequency(void);
static void *native_runtime;
static size_t require_native_console_borrows;
/* Shared runtime prefix owns the primary OwnerState pointer. Its first
 * member remains the checked MutatorContext ABI, not collector storage. */
static uint32_t *native_owner_context(void){
    assert(native_runtime);uintptr_t owner;memcpy(&owner,native_runtime,sizeof(owner));
    assert(owner&&!(owner&7));return (uint32_t *)owner;
}

typedef uint32_t (WINABI *WorkerEntry)(void *);
#ifndef _WIN32
typedef struct {
    int kind,signaled;
    pthread_t thread;
    unsigned joined;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    WorkerEntry entry;
    void *arg;
} Handle;
static pthread_t main_thread;
static pthread_mutex_t maps_lock=PTHREAD_MUTEX_INITIALIZER;
static struct {void *base;size_t size;int reserved;} maps[4096];
static void *start_thread(void *arg){
    Handle *h=arg;uint32_t result=h->entry(h->arg);assert(result==0);
    atomic_fetch_add(&worker_returns,1);return NULL;
}
static void *WINABI api_VirtualAlloc(void *address,size_t n,uint32_t flags,uint32_t prot){
    ABI_CHECK();assert(n&&!(n&4095));assert(pthread_mutex_lock(&maps_lock)==0);
    void *p=NULL;
    if(flags==0x1000){
        assert(address&&prot==4);int found=0;
        for(size_t i=0;i<4096;i++)if(maps[i].base&&(uintptr_t)address>=(uintptr_t)maps[i].base&&(uintptr_t)address+n<=(uintptr_t)maps[i].base+maps[i].size)found=1;
        assert(found&&mprotect(address,n,PROT_READ|PROT_WRITE)==0);p=address;
    }else{
        assert(!address&&(flags==0x2000||flags==0x3000));assert(prot==(flags==0x2000?1u:4u));
        p=mmap(NULL,n,flags==0x2000?PROT_NONE:PROT_READ|PROT_WRITE,MAP_PRIVATE|MAP_ANONYMOUS,-1,0);
        assert(p!=MAP_FAILED);size_t i=0;while(i<4096&&maps[i].base)i++;assert(i<4096);maps[i].base=p;maps[i].size=n;maps[i].reserved=flags==0x2000;
    }
    if(!native_runtime&&flags==0x3000)native_runtime=p;
    assert(pthread_mutex_unlock(&maps_lock)==0);return p;
}
static uint64_t WINABI api_VirtualFree(void *p,size_t n,uint32_t flags){
    ABI_CHECK();if(flags==0x8000&&fault==20){last_error=5;return dirty(0);}
    assert(pthread_mutex_lock(&maps_lock)==0);size_t i=0;
    while(i<4096&&(!maps[i].base||(uintptr_t)p<(uintptr_t)maps[i].base||(uintptr_t)p>=(uintptr_t)maps[i].base+maps[i].size))i++;
    /* VirtualFree(MEM_RELEASE) rejects an interior/non-base address without
     * releasing its reservation. The runtime's real OS-failure injection uses
     * that case; it must be a false return, not success or a model assertion.
     * https://learn.microsoft.com/windows/win32/api/memoryapi/nf-memoryapi-virtualfree */
    if(flags==0x8000&&(n||i==4096||p!=maps[i].base)){
        assert(pthread_mutex_unlock(&maps_lock)==0);last_error=487;return dirty(0);
    }
    assert(i<4096);
    if(flags==0x4000){assert(n&&(uintptr_t)p+n<=(uintptr_t)maps[i].base+maps[i].size);assert(mprotect(p,n,PROT_NONE)==0);assert(madvise(p,n,MADV_DONTNEED)==0);atomic_fetch_add(&decommits,1);}
    else{assert(flags==0x8000&&!n&&p==maps[i].base);assert(munmap(p,maps[i].size)==0);maps[i].base=NULL;}
    assert(pthread_mutex_unlock(&maps_lock)==0);return dirty(256);
}
#else
static DWORD main_thread;
static void *WINABI api_VirtualAlloc(void *p,size_t n,uint32_t f,uint32_t prot){void *r=VirtualAlloc(p,n,f,prot);if(!native_runtime&&f==0x3000)native_runtime=r;return r;}
static uint64_t WINABI api_VirtualFree(void *p,size_t n,uint32_t f){uint32_t ok=VirtualFree(p,n,f);if(ok&&f==0x4000)atomic_fetch_add(&decommits,1);return dirty(ok);}
#endif
typedef struct { void *thread,*event; unsigned joined,thread_closed,event_closed; } WorkerResource;
static WorkerResource worker_resources[2];
static void *event_handle;
static WorkerResource *resource_for(void *p){
    for(int i=1;i>=0;i--)if(worker_resources[i].thread==p||worker_resources[i].event==p)return &worker_resources[i];
    assert(0);return NULL;
}
static void *file_handles[128];
static void native_file_buffer(const void *,uint32_t);
static int file_index(void *p){if(!p)return -1;for(int i=0;i<128;i++)if(file_handles[i]==p)return i;return -1;}
static void native_file_boundary(void){
    assert(native_runtime);uint32_t *ctx=native_owner_context();
    assert(atomic_load((_Atomic uint32_t *)&ctx[0])==4&&atomic_load((_Atomic uint32_t *)&ctx[1])==2);
    fprintf(stderr,"win-native-file: state=2\n");
}
static void *WINABI api_CreateFileA(const char *path,uint32_t access,uint32_t share,void *security,uint32_t disposition,uint32_t attributes,void *template_file){
    ABI_CHECK();assert(share==3&&!security&&attributes==0x80&&!template_file);native_file_boundary();native_file_buffer(path,(uint32_t)strlen(path)+1);
#ifdef _WIN32
    void *p=CreateFileA(path,access,share,NULL,disposition,attributes,NULL);
    if(p==INVALID_HANDLE_VALUE){last_error=GetLastError();return p;}
#else
    int flags=access==0x80000000u?O_RDONLY:O_WRONLY|O_CREAT;
    if(disposition==2)flags|=O_TRUNC;
    if(access==4)flags|=O_APPEND;
    int fd=open(path,flags,0644);if(fd<0){last_error=(uint32_t)errno;return (void *)(uintptr_t)UINT64_MAX;}
    Handle *p=calloc(1,sizeof(*p));assert(p);p->kind=3;p->signaled=fd;
#endif
    int i=0;while(i<128&&file_handles[i])i++;assert(i<128);file_handles[i]=p;return p;
}
static void native_file_buffer(const void *p,uint32_t n){
#ifndef _WIN32
    assert(pthread_mutex_lock(&maps_lock)==0);
    for(size_t i=0;i<4096;i++)if(maps[i].base&&maps[i].reserved){
        uintptr_t lo=(uintptr_t)maps[i].base,hi=lo+maps[i].size,b=(uintptr_t)p;
        assert(!(lo<=b&&b<hi)&&!(n&&b<lo&&lo<b+n));
    }
    assert(pthread_mutex_unlock(&maps_lock)==0);
#else
    (void)p;(void)n;
#endif
}
/* Hold the owner inside the ReadFile API model while the emitted worker runs.
 * Freeze the emitted clock until this point so workload speed cannot consume
 * the uncommit delay. This is progress evidence, not native read latency. */
static void native_read_wait(void){
    static int waited;if((fault!=49&&fault!=50)||waited)return;waited=1;
    unsigned before=atomic_load(&decommits),wake=atomic_load(&signals),ticks=atomic_load(&timeouts);
    uint64_t started=raw_counter(),frequency=counter_frequency();
    uint64_t phase=atomic_load((_Atomic uint64_t *)((char *)native_owner_context()+16));
    atomic_store(&idle_clock_origin,started);
    while((atomic_load(&decommits)==before||atomic_load(&timeouts)==ticks)&&raw_counter()-started<12*frequency){
#ifdef _WIN32
        Sleep(20);
#else
        struct timespec pause={0,20000000};nanosleep(&pause,NULL);
#endif
        uint32_t *ctx=native_owner_context();assert(atomic_load((_Atomic uint32_t *)&ctx[1])==2);
        assert(atomic_load(&signals)==wake);
        assert(atomic_load((_Atomic uint64_t *)((char *)native_owner_context()+16))==phase);
    }
    unsigned after=atomic_load(&decommits);
    fprintf(stderr,"win-native-read: progress=%u before=%u after=%u no-owner-signal=%u timeouts=%u\n",after>before,before,after,atomic_load(&signals)==wake,atomic_load(&timeouts)-ticks);fflush(stderr);
    assert(after>before&&atomic_load(&timeouts)>ticks);
}
static uint64_t WINABI api_ReadFile(void *p,void *data,uint32_t n,uint32_t *read_count,void *overlapped){
    ABI_CHECK();assert(read_count&&!overlapped);int fi=file_index(p);
#ifdef _WIN32
    int is_stdin=p==GetStdHandle(STD_INPUT_HANDLE);
#else
    int is_stdin=!p;
#endif
    assert(fi>=0||is_stdin);native_file_boundary();native_file_buffer(data,n);native_read_wait();
    if((fi>=0&&fault==41)||(is_stdin&&fault>=45&&fault<=47)){
        last_error=fault==46?109:fault==47?38:5;*read_count=0;return dirty(0);
    }
    if(is_stdin&&fault==48&&n>3)n=3;
#ifdef _WIN32
    return dirty(ReadFile(p,data,n,(DWORD *)read_count,NULL));
#else
    ssize_t out=read(is_stdin?0:((Handle *)p)->signaled,data,n);
    if(out<0){last_error=(uint32_t)errno;*read_count=0;return dirty(0);}
    *read_count=(uint32_t)out;return dirty(1);
#endif
}

static uint64_t WINABI api_CopyFileA(const char *source,const char *target,uint32_t fail_exists){
    ABI_CHECK();assert(!fail_exists);native_file_boundary();native_file_buffer(source,(uint32_t)strlen(source)+1);native_file_buffer(target,(uint32_t)strlen(target)+1);
    if(fault==44){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t result=CopyFileA(source,target,FALSE);if(!result)last_error=GetLastError();return dirty(result);
#else
    int input=open(source,O_RDONLY);if(input<0){last_error=(uint32_t)errno;return dirty(0);}
    int output=open(target,O_WRONLY|O_CREAT|O_TRUNC,0644);int ok=output>=0;
    char buffer[8192];ssize_t n=0;
    while(ok&&(n=read(input,buffer,sizeof(buffer)))>0){
        ssize_t at=0;while(at<n){ssize_t k=write(output,buffer+at,(size_t)(n-at));if(k<=0){ok=0;break;}at+=k;}
    }
    if(ok&&n<0)ok=0;
    if(close(input))ok=0;
    if(output>=0&&close(output))ok=0;
    if(!ok)last_error=(uint32_t)errno;
    return dirty((uint32_t)ok);
#endif
}
/* Filesystem operations receive native paths and output buffers only. */
static void fs_boundary(const char *path){
    native_file_boundary();atomic_fetch_add(&filesystem_calls,1);
    if(path)native_file_buffer(path,(uint32_t)strlen(path)+1);
    fprintf(stderr,"win-native-fs: state=2\n");
}
#ifndef _WIN32
static uint32_t fs_error(void){return errno==ENOENT?2:errno==EEXIST?183:5;}
#endif
static uint64_t WINABI api_GetFileAttributesA(const char *path){
    ABI_CHECK();fs_boundary(path);if(fault==51){last_error=5;return dirty(UINT32_MAX);}
#ifdef _WIN32
    uint32_t r=GetFileAttributesA(path);if(r==UINT32_MAX)last_error=GetLastError();return dirty(r);
#else
    struct stat st;if(stat(path,&st)){last_error=fs_error();return dirty(UINT32_MAX);}
    return dirty(S_ISDIR(st.st_mode)?0x10:0x80);
#endif
}
static uint64_t WINABI api_GetCurrentDirectoryA(uint32_t n,char *buf){
    ABI_CHECK();fs_boundary(NULL);native_file_buffer(buf,n);
    if(fault==52){last_error=5;return dirty(0);}if(fault==53)return dirty(n+8);
#ifdef _WIN32
    uint32_t r=GetCurrentDirectoryA(n,buf);if(!r)last_error=GetLastError();return dirty(r);
#else
    char *cwd=getcwd(NULL,0);assert(cwd);size_t len=strlen(cwd);uint32_t r=(uint32_t)(len<n?len:len+1);
    if(len<n)memcpy(buf,cwd,len+1);free(cwd);return dirty(r);
#endif
}
static uint64_t WINABI api_CreateDirectoryA(const char *path,void *security){
    ABI_CHECK();assert(!security);fs_boundary(path);if(fault==54){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=CreateDirectoryA(path,NULL);if(!r)last_error=GetLastError();return dirty(r);
#else
    if(mkdir(path,0755)){last_error=fs_error();return dirty(0);}return dirty(256);
#endif
}
static uint64_t WINABI api_RemoveDirectoryA(const char *path){
    ABI_CHECK();fs_boundary(path);if(fault==55){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=RemoveDirectoryA(path);if(!r)last_error=GetLastError();return dirty(r);
#else
    if(rmdir(path)){last_error=fs_error();return dirty(0);}return dirty(256);
#endif
}
static uint64_t WINABI api_DeleteFileA(const char *path){
    ABI_CHECK();fs_boundary(path);if(fault==57){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=DeleteFileA(path);if(!r)last_error=GetLastError();return dirty(r);
#else
    if(unlink(path)){last_error=fs_error();return dirty(0);}return dirty(256);
#endif
}
static uint64_t WINABI api_MoveFileExA(const char *src,const char *dst,uint32_t flags){
    ABI_CHECK();assert(flags==1);fs_boundary(src);native_file_buffer(dst,(uint32_t)strlen(dst)+1);
    if(fault==56){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=MoveFileExA(src,dst,flags);if(!r)last_error=GetLastError();return dirty(r);
#else
    if(rename(src,dst)){last_error=fs_error();return dirty(0);}return dirty(256);
#endif
}
#ifndef _WIN32
typedef struct {DIR *directory;char *path;} FindHandle;
static uint64_t find_next(FindHandle *h,unsigned char *data){
    errno=0;struct dirent *entry=readdir(h->directory);if(!entry){last_error=errno?fs_error():18;return dirty(0);}
    memset(data,0,320);size_t n=strlen(entry->d_name);assert(n<260);memcpy(data+44,entry->d_name,n+1);return dirty(256);
}
#endif
static void *WINABI api_FindFirstFileA(const char *pattern,void *data){
    ABI_CHECK();fs_boundary(pattern);native_file_buffer(data,320);
    if(fault==60){last_error=5;return (void *)(uintptr_t)UINT64_MAX;}
#ifdef _WIN32
    void *r=FindFirstFileA(pattern,data);if(r==INVALID_HANDLE_VALUE)last_error=GetLastError();else atomic_fetch_add(&searches_open,1);return r;
#else
    size_t n=strlen(pattern);assert(n>=2&&pattern[n-1]=='*');char *path=strdup(pattern);assert(path);path[n-2]=0;
    for(size_t i=0;i<n-2;i++)if(path[i]=='\\')path[i]='/';
    DIR *dir=opendir(path);if(!dir){free(path);last_error=fs_error();return (void *)(uintptr_t)UINT64_MAX;}
    FindHandle *h=malloc(sizeof(*h));assert(h);h->directory=dir;h->path=path;
    if(!(uint32_t)find_next(h,data)){closedir(dir);free(path);free(h);return (void *)(uintptr_t)UINT64_MAX;}atomic_fetch_add(&searches_open,1);return h;
#endif
}
static uint64_t WINABI api_FindNextFileA(void *handle,void *data){
    ABI_CHECK();fs_boundary(NULL);native_file_buffer(data,320);if(fault==58){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=FindNextFileA(handle,data);if(!r)last_error=GetLastError();return dirty(r);
#else
    return find_next(handle,data);
#endif
}
static uint64_t WINABI api_FindClose(void *handle){
    ABI_CHECK();fs_boundary(NULL);if(fault==59){last_error=5;return dirty(0);}
#ifdef _WIN32
    uint32_t r=FindClose(handle);if(!r)last_error=GetLastError();else atomic_fetch_sub(&searches_open,1);return dirty(r);
#else
    FindHandle *h=handle;int r=closedir(h->directory);free(h->path);free(h);if(r){last_error=fs_error();return dirty(0);}atomic_fetch_sub(&searches_open,1);return dirty(256);
#endif
}

static void *WINABI api_CreateEventA(void *a,int manual,int signaled,const char *name){
    ABI_CHECK();assert(!a&&!manual&&!signaled&&!name);if(fault==1){last_error=8;return NULL;}
    unsigned resource_index=atomic_fetch_add(&events,1);assert(resource_index<2);
#ifdef _WIN32
    event_handle=CreateEventA(NULL,FALSE,FALSE,NULL);worker_resources[resource_index].event=event_handle;return event_handle;
#else
    Handle *h=calloc(1,sizeof(*h));assert(h);h->kind=2;assert(pthread_mutex_init(&h->mutex,NULL)==0);assert(pthread_cond_init(&h->cond,NULL)==0);event_handle=h;worker_resources[resource_index].event=h;return h;
#endif
}
static void *WINABI api_CreateThread(void *a,size_t n,WorkerEntry entry,void *arg,uint32_t flags,uint32_t *id){
    ABI_CHECK();assert(!a&&n==256*1024&&entry&&arg&&flags==0x10000&&!id);
    if(fault==2||fault==3||fault==50){last_error=8;return NULL;}atomic_fetch_add(&threads,1);
#ifdef _WIN32
    void *created=CreateThread(NULL,n,(LPTHREAD_START_ROUTINE)entry,arg,flags,NULL);
#else
    Handle *h=calloc(1,sizeof(*h));assert(h);h->kind=1;h->entry=entry;h->arg=arg;assert(pthread_create(&h->thread,NULL,start_thread,h)==0);void *created=h;
#endif
    if(fault==11){
        unsigned spin=0;
        while(!atomic_load(&waits)&&spin++<10000){
#ifdef _WIN32
            Sleep(1);
#else
            struct timespec pause={0,1000000};nanosleep(&pause,NULL);
#endif
        }
        assert(atomic_load(&waits)>0);
    }
    resource_for(event_handle)->thread=created;return created;
}
static uint64_t WINABI api_CloseHandle(void *p){
    ABI_CHECK();assert(p);
    int fi=file_index(p);
    if(fi>=0){
        native_file_boundary();if(fault==43){last_error=5;return dirty(0);}file_handles[fi]=NULL;
#ifdef _WIN32
        return dirty(CloseHandle(p));
#else
        Handle *h=p;int status=close(h->signaled);free(h);return dirty(status==0);
#endif
    }
    atomic_fetch_add(&closes,1);
    WorkerResource *resource=resource_for(p);
    if(fault==3||(fault==14&&p==resource->thread)||(fault==15&&p==resource->event)){last_error=5;return dirty(0);}
    if(p==resource->thread){assert(resource->joined&&!resource->thread_closed);resource->thread_closed=1;atomic_fetch_add(&thread_closes,1);}
    else{assert(p==resource->event&&!resource->event_closed);if(resource->thread)assert(resource->joined&&resource->thread_closed);resource->event_closed=1;atomic_fetch_add(&event_closes,1);}
#ifdef _WIN32
    return dirty(CloseHandle(p));
#else
    Handle *h=p;
    if(h->kind==1){assert(h->joined);}
    else{assert(h->kind==2);assert(pthread_cond_destroy(&h->cond)==0);assert(pthread_mutex_destroy(&h->mutex)==0);}
    free(h);return dirty(256);
#endif
}
static uint64_t WINABI api_SetEvent(void *p){
    ABI_CHECK();assert(p);atomic_fetch_add(&signals,1);if(fault==5){last_error=5;return dirty(0);}
#ifdef _WIN32
    return dirty(SetEvent(p));
#else
    Handle *h=p;assert(h->kind==2);assert(pthread_mutex_lock(&h->mutex)==0);h->signaled=1;assert(pthread_cond_signal(&h->cond)==0);assert(pthread_mutex_unlock(&h->mutex)==0);return dirty(256);
#endif
}
static uint64_t WINABI api_WaitForSingleObject(void *p,uint32_t milliseconds){
    ABI_CHECK();assert(p);
    if(milliseconds==UINT32_MAX){
        WorkerResource *resource=resource_for(p);assert(p==resource->thread&&!resource->joined);assert(atomic_load(&signals)>0);atomic_fetch_add(&join_waits,1);
        if(fault==13){last_error=5;return dirty(UINT32_MAX);}
#ifdef _WIN32
        uint32_t result=WaitForSingleObject(p,milliseconds);assert(result==0);
#else
        Handle *h=p;assert(h->kind==1&&!h->joined);assert(pthread_join(h->thread,NULL)==0);h->joined=1;
        assert(atomic_load(&worker_returns)>0);
#endif
        resource->joined=1;atomic_fetch_add(&joined,1);return dirty(0);
    }
    assert(milliseconds==1000);atomic_fetch_add(&waits,1);
#ifdef _WIN32
    if(GetCurrentThreadId()==main_thread)atomic_fetch_add(&main_waits,1);
#else
    if(pthread_equal(pthread_self(),main_thread))atomic_fetch_add(&main_waits,1);
#endif
    if(fault==4){last_error=5;return dirty(UINT32_MAX);}
#ifdef _WIN32
    uint32_t result=WaitForSingleObject(p,milliseconds);if(result==258)atomic_fetch_add(&timeouts,1);if(result==0)atomic_fetch_add(&notified,1);return dirty(result);
#else
    Handle *h=p;assert(h->kind==2);struct timespec t;assert(clock_gettime(CLOCK_REALTIME,&t)==0);t.tv_sec++;
    assert(pthread_mutex_lock(&h->mutex)==0);int result=0;
    while(!h->signaled&&!result)result=pthread_cond_timedwait(&h->cond,&h->mutex,&t);
    uint32_t value=0;if(h->signaled)h->signaled=0;else{assert(result==ETIMEDOUT);value=258;atomic_fetch_add(&timeouts,1);}
    if(value==0)atomic_fetch_add(&notified,1);
    assert(pthread_mutex_unlock(&h->mutex)==0);return dirty(value);
#endif
}
// The idle fixture gives already-retired pages a fresh measured interval.
// Freeze its clock before owner blocking, then use real elapsed native time;
// workload/scheduler duration cannot consume the uncommit delay in advance.
static uint64_t raw_counter(void){
#ifdef _WIN32
    LARGE_INTEGER n;assert(QueryPerformanceCounter(&n));return (uint64_t)n.QuadPart;
#else
    struct timespec t;assert(clock_gettime(CLOCK_MONOTONIC,&t)==0);return (uint64_t)t.tv_sec*1000000000+(uint64_t)t.tv_nsec;
#endif
}
static uint64_t counter_frequency(void){
#ifdef _WIN32
    LARGE_INTEGER n;assert(QueryPerformanceFrequency(&n));return (uint64_t)n.QuadPart;
#else
    return 1000000000;
#endif
}
static uint64_t WINABI api_QueryPerformanceCounter(int64_t *n){
    ABI_CHECK();assert(n);atomic_fetch_add(&clocks,1);if(fault==6){last_error=5;return dirty(0);}
    if(fault==8||fault==49||fault==50){
        uint64_t origin=atomic_load(&idle_clock_origin);
        *n=(int64_t)(counter_frequency()+(origin?raw_counter()-origin:0));return dirty(1);
    }
#ifdef _WIN32
    return dirty(QueryPerformanceCounter((LARGE_INTEGER *)n));
#else
    struct timespec t;assert(clock_gettime(CLOCK_MONOTONIC,&t)==0);*n=(int64_t)t.tv_sec*1000000000+t.tv_nsec;return dirty(1);
#endif
}
static uint64_t WINABI api_QueryPerformanceFrequency(int64_t *n){
    ABI_CHECK();assert(n);
#ifdef _WIN32
    return dirty(QueryPerformanceFrequency((LARGE_INTEGER *)n));
#else
    *n=1000000000;return dirty(1);
#endif
}
static uint64_t WINABI api_GetLastError(void){return dirty(last_error);}
static void *WINABI api_GetStdHandle(uint32_t which){
#ifdef _WIN32
    return GetStdHandle(which);
#else
    assert(which==(uint32_t)-11||which==(uint32_t)-12||which==(uint32_t)-10);return (void *)(uintptr_t)(which==(uint32_t)-11?1:which==(uint32_t)-12?2:0);
#endif
}
static uint64_t WINABI api_WriteFile(void *fd,const void *data,uint32_t n,uint32_t *written,void *overlapped){
    ABI_CHECK();assert(!overlapped&&written);
    if(require_native_console_borrows){
        uint32_t *ctx=native_owner_context();
        assert(ctx[0]==4&&atomic_load((_Atomic uint32_t *)(ctx+1))==2);
        assert(atomic_load((_Atomic uint64_t *)((char *)native_runtime+require_native_console_borrows))==0);
        native_file_buffer(data,n);
    }
    if(file_index(fd)>=0){native_file_boundary();native_file_buffer(data,n);if(fault==42){last_error=5;uint32_t zero=0;memcpy(written,&zero,sizeof(zero));return dirty(0);}}
#ifdef _WIN32
    return dirty(WriteFile(fd,data,n,(DWORD *)written,NULL));
#else
    int native_fd=file_index(fd)>=0?((Handle *)fd)->signaled:(int)(uintptr_t)fd;
    ssize_t out=write(native_fd,data,n);
    if(out<0){last_error=errno==EBADF?6:5;uint32_t zero=0;memcpy(written,&zero,sizeof(zero));return dirty(0);}
    uint32_t count=(uint32_t)out;memcpy(written,&count,sizeof(count));return dirty(1);
#endif
}
static uint64_t WINABI api_GetEnvironmentVariableA(const char *key,char *out,uint32_t capacity){
    ABI_CHECK();assert(!strcmp(key,"KLASSIC_GC_IDLE"));
    if(fault==9){last_error=203;return dirty(0);}
    if(!out||capacity<8)return dirty(8);
    if(fault==8){
        atomic_store(&idle_clock_origin,raw_counter());
        unsigned before=atomic_load(&decommits),wake=atomic_load(&signals),ticks=atomic_load(&timeouts);
        // Observe progress, not completion by one arbitrarily scheduled wakeup.
        // The owner never signals or touches managed memory while blocked.
        // This is a bounded functional probe, not a 5-second latency target.
        uint64_t started=raw_counter(),frequency=counter_frequency();
        unsigned after;
        do {
#ifdef _WIN32
            Sleep(20);
#else
            struct timespec pause={0,20000000};while(nanosleep(&pause,&pause)&&errno==EINTR){}
#endif
            after=atomic_load(&decommits);
            if(after>before&&atomic_load(&timeouts)>ticks)break;
        } while(raw_counter()-started<15*frequency);
        fprintf(stderr,"win-production-idle: before=%u after=%u no-owner-signal=%u elapsed_ms=%llu timeouts=%u\n",before,after,atomic_load(&signals)==wake,(unsigned long long)((raw_counter()-started)*1000/frequency),atomic_load(&timeouts)-ticks);fflush(stderr);
        assert(before==0&&after>before);assert(atomic_load(&signals)==wake);assert(atomic_load(&timeouts)>ticks);
        assert(!strcmp(key,"KLASSIC_GC_IDLE"));
    }
    memcpy(out,"resumed",8);return dirty(7);
}
static const char *WINABI api_GetCommandLineA(void){return "klassic-worker-model";}
static const char *WINABI api_GetEnvironmentStringsA(void){return "\0";}
static _Noreturn void WINABI api_ExitProcess(uint32_t code){
    ABI_CHECK();if(native_runtime){uint32_t *ctx=native_owner_context();assert(atomic_load((_Atomic uint32_t *)&ctx[1])==0);}
    unsigned e=atomic_load(&events),t=atomic_load(&threads),c=atomic_load(&closes);
    if(!code){assert(atomic_load(&tls_keys_active)==0);assert(atomic_load(&searches_open)==0);assert(fault==49||fault==46||fault==47||fault==48||fault==0||fault==1||fault==2||fault==8||fault==10||fault==11||fault==12||fault==16||fault==17);assert(e==(fault==1?0u:2u));assert(t==((fault==1||fault==2)?0u:2u));assert(c==(fault==1?0u:fault==2?2u:4u));assert(atomic_load(&joined)==t);assert(atomic_load(&thread_closes)==t);assert(atomic_load(&event_closes)==e);}
    if(fault==18||fault==19){assert(code==(fault==18?7u:23u));assert(e==2&&t==2&&c==4);assert(atomic_load(&joined)==2&&atomic_load(&thread_closes)==2&&atomic_load(&event_closes)==2);}
    if(!code&&fault==12){assert(atomic_load(&yields)>0);}
    if(!code&&(fault==10||fault==16)){assert(atomic_load(&main_waits)==2);assert(atomic_load(&timeouts)>0);}
    fprintf(stderr,"win-production: events=%u threads=%u waits=%u timeouts=%u signals=%u closes=%u main_waits=%u notified=%u clocks=%u yields=%u join_waits=%u joined=%u thread_closes=%u event_closes=%u\n",e,t,atomic_load(&waits),atomic_load(&timeouts),atomic_load(&signals),c,atomic_load(&main_waits),atomic_load(&notified),atomic_load(&clocks),atomic_load(&yields),atomic_load(&join_waits),atomic_load(&joined),atomic_load(&thread_closes),atomic_load(&event_closes));fflush(stderr);
#ifdef _WIN32
    ExitProcess(code);
#else
    _Exit((int)code);
#endif
}
#ifndef _WIN32
static _Noreturn void WINABI unsupported(void){fprintf(stderr,"unmodeled Windows import called\n");fflush(stderr);
#ifdef _WIN32
    ExitProcess(126);
#else
    _Exit(126);
#endif
}
#endif
static void WINABI api_Sleep(uint32_t millis){
    ABI_CHECK();
    if(millis){
        assert(native_runtime);
        uint32_t *ctx=native_owner_context();
        assert(ctx[0]==4&&atomic_load((_Atomic uint32_t *)(ctx+1))==2);
        fprintf(stderr,"win-native-sleep: millis=%u state=2\n",millis);
#ifdef _WIN32
        Sleep(millis);
#else
        struct timespec delay={millis/1000,(long)(millis%1000)*1000000};
        while(nanosleep(&delay,&delay)&&errno==EINTR){}
#endif
        assert(atomic_load((_Atomic uint32_t *)(ctx+1))==2);
        return;
    }
    atomic_fetch_add(&yields,1);
#ifdef _WIN32
    Sleep(0);
#else
    assert(sched_yield()==0);
    __asm__ volatile("pcmpeqd %%xmm0,%%xmm0\n\tpcmpeqd %%xmm1,%%xmm1\n\tpcmpeqd %%xmm2,%%xmm2\n\tpcmpeqd %%xmm3,%%xmm3\n\tpcmpeqd %%xmm4,%%xmm4\n\tpcmpeqd %%xmm5,%%xmm5" ::: "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5");
#endif
}
/* Model actual per-OS-thread key values, with the same Win64 native ABI. */
#ifndef _WIN32
static pthread_mutex_t tls_key_lock=PTHREAD_MUTEX_INITIALIZER;
static pthread_key_t tls_keys[64];
static unsigned tls_key_live[64];
#endif
static uint64_t WINABI api_GetCurrentThreadId(void){
    ABI_CHECK();
#ifdef _WIN32
    return dirty(GetCurrentThreadId());
#else
    static _Atomic uint32_t next_id;static _Thread_local uint32_t id;
    if(!id)id=atomic_fetch_add(&next_id,1)+1;
    assert(id);return dirty(id);
#endif
}
static uint64_t WINABI api_TlsAlloc(void){
    ABI_CHECK();
#ifdef _WIN32
    uint32_t key=TlsAlloc();if(key!=UINT32_MAX)atomic_fetch_add(&tls_keys_active,1);return dirty(key);
#else
    pthread_mutex_lock(&tls_key_lock);
    for(unsigned i=0;i<64;i++)if(!tls_key_live[i]&&!pthread_key_create(&tls_keys[i],NULL)){
        tls_key_live[i]=1;atomic_fetch_add(&tls_keys_active,1);pthread_mutex_unlock(&tls_key_lock);return dirty(i);
    }
    pthread_mutex_unlock(&tls_key_lock);last_error=8;return dirty(UINT32_MAX);
#endif
}
static uint64_t WINABI api_TlsFree(uint32_t key){
    ABI_CHECK();
#ifdef _WIN32
    uint32_t ok=TlsFree(key);if(ok)atomic_fetch_sub(&tls_keys_active,1);return dirty(ok);
#else
    pthread_mutex_lock(&tls_key_lock);
    int ok=key<64&&tls_key_live[key]&&!pthread_key_delete(tls_keys[key]);
    if(ok){tls_key_live[key]=0;atomic_fetch_sub(&tls_keys_active,1);}else last_error=87;
    pthread_mutex_unlock(&tls_key_lock);return dirty(ok);
#endif
}
static uint64_t WINABI api_TlsSetValue(uint32_t key,void *value){
    ABI_CHECK();
#ifdef _WIN32
    return dirty(TlsSetValue(key,value));
#else
    pthread_mutex_lock(&tls_key_lock);
    int ok=key<64&&tls_key_live[key]&&!pthread_setspecific(tls_keys[key],value);
    if(!ok)last_error=87;
    pthread_mutex_unlock(&tls_key_lock);return dirty(ok);
#endif
}
static void *WINABI api_TlsGetValue(uint32_t key){
    ABI_CHECK();
#ifdef _WIN32
    void *value=TlsGetValue(key);last_error=GetLastError();return value;
#else
    pthread_mutex_lock(&tls_key_lock);void *value=NULL;
    if(key<64&&tls_key_live[key]){value=pthread_getspecific(tls_keys[key]);last_error=0;}else last_error=87;
    pthread_mutex_unlock(&tls_key_lock);return value;
#endif
}
static void *resolve(const char *name){
#define API(n) if(!strcmp(name,#n))return (void *)api_##n
    API(GetFileAttributesA);API(GetCurrentDirectoryA);API(CreateDirectoryA);API(RemoveDirectoryA);API(DeleteFileA);API(MoveFileExA);API(FindFirstFileA);API(FindNextFileA);API(FindClose);
    API(GetCurrentThreadId);API(TlsAlloc);API(TlsFree);API(TlsSetValue);API(TlsGetValue);
    API(CopyFileA);API(CreateFileA);API(ReadFile);API(Sleep);API(VirtualAlloc);API(VirtualFree);API(CreateEventA);API(CreateThread);API(CloseHandle);API(SetEvent);API(WaitForSingleObject);API(QueryPerformanceCounter);API(QueryPerformanceFrequency);API(GetLastError);API(GetStdHandle);API(WriteFile);API(ExitProcess);API(GetCommandLineA);API(GetEnvironmentStringsA);API(GetEnvironmentVariableA);
#undef API
#ifdef _WIN32
    void *p=(void *)GetProcAddress(GetModuleHandleA("kernel32.dll"),name);assert(p);return p;
#else
    return (void *)unsupported;
#endif
}
