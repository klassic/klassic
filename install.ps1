# Klassic installer for Windows:
#
#   irm https://raw.githubusercontent.com/klassic/klassic/main/install.ps1 | iex
#
# Downloads the latest release klassic.exe into ~\.klassic\bin. Set
# KLASSIC_VERSION to pin a tag, KLASSIC_HOME to change the install
# root. Mirrors install.sh, including the dogfooding smoke test: the
# freshly installed compiler verifies itself by running a Klassic
# program.
$ErrorActionPreference = "Stop"

$repo = "klassic/klassic"
$root = if ($env:KLASSIC_HOME) { $env:KLASSIC_HOME } else { Join-Path $env:USERPROFILE ".klassic" }
$installDir = Join-Path $root "bin"

if ([System.Environment]::Is64BitOperatingSystem -eq $false) {
    throw "no prebuilt binary for 32-bit Windows (try: cargo install --git https://github.com/$repo)"
}

$tag = $env:KLASSIC_VERSION
if (-not $tag) {
    $tag = (Invoke-RestMethod "https://api.github.com/repos/$repo/releases/latest").tag_name
}
if (-not $tag) {
    throw "could not determine the latest release tag"
}

$asset = "klassic-$tag-x86_64-pc-windows-msvc.zip"
$url = "https://github.com/$repo/releases/download/$tag/$asset"
Write-Host "downloading klassic $tag (x86_64-pc-windows-msvc)..."
New-Item -ItemType Directory -Force -Path $installDir | Out-Null
$zip = Join-Path $env:TEMP "klassic-install-$PID.zip"
Invoke-WebRequest $url -OutFile $zip
Expand-Archive -Path $zip -DestinationPath $installDir -Force
Remove-Item $zip

# Dogfooding smoke: the freshly installed compiler verifies itself by
# running a Klassic program. A probe file (not -e) sidesteps Windows
# PowerShell 5.1's native-argument quote mangling.
$klassic = Join-Path $installDir "klassic.exe"
$probe = Join-Path $env:TEMP "klassic-install-probe-$PID.kl"
Set-Content -Path $probe -Value 'println("klassic " + "is " + "alive")' -Encoding Ascii
$alive = & $klassic $probe
Remove-Item $probe
if ($alive -ne "klassic is alive") {
    throw "installed klassic.exe failed its self-check (got: $alive)"
}

$version = & $klassic --version
Write-Host "installed: $version -> $installDir"

$onPath = ($env:Path -split ";") -contains $installDir
if (-not $onPath) {
    Write-Host ""
    Write-Host "add it to your PATH (persists for your user):"
    Write-Host "  [Environment]::SetEnvironmentVariable('Path', [Environment]::GetEnvironmentVariable('Path', 'User') + ';$installDir', 'User')"
}
