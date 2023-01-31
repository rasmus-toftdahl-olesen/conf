function Has-Path($Path) {
    $CurrentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ( $CurrentPath.ToLower().Contains($Path.ToLower()) )
    {
        return $True;
    }
    else
    {
        return $False;
    }
}


function Add-Path($Path) {
    $Path = [Environment]::GetEnvironmentVariable("PATH", "User") + [IO.Path]::PathSeparator + $Path
    [Environment]::SetEnvironmentVariable( "Path", $Path, "User" )
}

function Add-Path-If-Not-Exists($Path) {
    if ( -Not(Has-Path($Path)) )
    {
        Write-Host "Will add PATH $Path";
        Add-Path($Path);
    }
}

function Main($Path)  {
    Add-Path-If-Not-Exists($Path);
}
