$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = __IMAGE__
$psi.UseShellExecute = $false
$psi.CreateNoWindow = $true
$psi.RedirectStandardOutput = $true
$psi.RedirectStandardError = $true
$psi.RedirectStandardInput = __HAS_STDIN__
$psi.WorkingDirectory = [System.IO.Path]::GetTempPath()
$p = New-Object System.Diagnostics.Process
$p.StartInfo = $psi
try {
    $watch = [System.Diagnostics.Stopwatch]::StartNew()
    $started = $p.Start()
    $mask = 0
    if (__AFFINE__) {
        $p.ProcessorAffinity = [IntPtr]1
        $mask = $p.ProcessorAffinity.ToInt64()
    }
    $errors = $p.StandardError.ReadToEndAsync()
    $prefix = ''
    if (__IDLE__) {
        $first = $p.StandardOutput.ReadLineAsync()
        if (-not $first.Wait(60000)) { throw 'native first-output timeout' }
        $line = $first.Result
        if ($null -ne $line) { $prefix = $line + "`n" }
    }
    $tail = $p.StandardOutput.ReadToEndAsync()
    __STDIN__
    $samples = @()
    if (__IDLE__) {
        for ($i = 0; $i -lt 9; $i++) {
            if ($p.HasExited) { throw 'owner exited before idle samples completed' }
            $p.Refresh()
            $samples += [long]$p.PrivateMemorySize64
            if ($i -lt 8) { Start-Sleep -Milliseconds 500 }
        }
    }
    if (-not $p.WaitForExit(60000)) { throw 'native exit timeout' }
    [Console]::Out.Write($prefix + $tail.GetAwaiter().GetResult())
    [Console]::Error.Write($errors.GetAwaiter().GetResult())
    $drop = 0
    if (__IDLE__) {
        $early = ($samples[0..2] | Measure-Object -Maximum).Maximum
        $late = ($samples[3..8] | Measure-Object -Minimum).Minimum
        $drop = [long]($early - $late)
    }
    [Console]::Error.WriteLine('win-native: affinity=' + $mask + ' private_drop=' + $drop + ' samples=' + ($samples -join ','))
    $code = $p.ExitCode
} finally {
    if ($started -and -not $p.HasExited) { $p.Kill(); $p.WaitForExit() }
    $p.Dispose()
}
exit $code
