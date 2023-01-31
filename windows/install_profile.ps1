$scripts = "$PSScriptRoot\powershell"
$content = "`$items = Get-ChildItem -Path $scripts"
$content += "`nforeach(`$item in `$items) { Import-Module -Name `"$scripts\`$item`" -Verbose }"
Write-Output $content | Out-File -FilePath $profile

Get-Content $profile
