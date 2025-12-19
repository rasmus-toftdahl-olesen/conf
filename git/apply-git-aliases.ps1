param(
    [string]$ConfigPath = "alias.cfg"
)

# Resolve relative path from script directory
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$fullPath = Join-Path $scriptDir $ConfigPath

if (-not (Test-Path $fullPath)) {
    Write-Error "Config file not found: $fullPath"
    exit 1
}

$content = Get-Content $fullPath -Raw

# Parse alias definitions
$aliases = @{}
foreach ($line in $content -split "`n") {
    $line = $line.Trim()
    
    # Skip empty lines and comments
    if ([string]::IsNullOrWhiteSpace($line) -or $line.StartsWith("#")) {
        continue
    }
    
    # Skip section headers like [alias]
    if ($line.StartsWith("[") -and $line.EndsWith("]")) {
        continue
    }
    
    # Parse alias = value format
    if ($line -match "^\s*(\w+)\s*=\s*(.+)$") {
        $aliasName = $matches[1]
        $aliasValue = $matches[2].Trim()
        $aliases[$aliasName] = $aliasValue
    }
}

# Apply aliases to git config
foreach ($name in $aliases.Keys) {
    $value = $aliases[$name]
    Write-Host "Applying alias: git $name -> $value"
    & git config --global alias.$name $value
}

Write-Host "`nAll aliases applied successfully!" -ForegroundColor Green
