param([Parameter(Mandatory=$true)][string]$FixtureDirectory)
$ErrorActionPreference='Stop'
$ProgressPreference='SilentlyContinue'
$base=$FixtureDirectory
$records=@()
foreach($affine in @($false,$true)) {
 foreach($mode in 0..3) {
  $p=New-Object System.Diagnostics.Process
  $p.StartInfo.FileName=Join-Path $base ("pin-"+$mode+".exe")
  $p.StartInfo.UseShellExecute=$false
  $p.StartInfo.CreateNoWindow=$true
  $p.StartInfo.RedirectStandardOutput=$true
  $p.StartInfo.RedirectStandardError=$true
  $started=$false
  try {
   $watch=[System.Diagnostics.Stopwatch]::StartNew()
   $started=$p.Start()
   $mask=0
   if($affine){$p.ProcessorAffinity=[IntPtr]1;$mask=$p.ProcessorAffinity.ToInt64()}
   $stdout=$p.StandardOutput.ReadToEndAsync()
   $stderr=$p.StandardError.ReadToEndAsync()
   if(-not $p.WaitForExit(60000)){throw 'native pin callback exit timeout'}
   $out=$stdout.GetAwaiter().GetResult()
   $err=$stderr.GetAwaiter().GetResult()
   $expected=[System.IO.File]::ReadAllText((Join-Path $base ("pin-"+$mode+".expected")))
   if($p.ExitCode -ne 0 -or $out -ne $expected){throw ("mode="+$mode+" affine="+$affine+" exit="+$p.ExitCode+" stdout="+$out+" stderr="+$err)}
   $records+=@{mode=$mode;affinity=$mask;exit=$p.ExitCode;stdout=$out;stderr=$err;elapsed_ms=$watch.ElapsedMilliseconds}
  } finally {
   if($started -and -not $p.HasExited){$p.Kill();$p.WaitForExit()}
   $p.Dispose()
  }
 }
}
$records | ConvertTo-Json -Depth 4
