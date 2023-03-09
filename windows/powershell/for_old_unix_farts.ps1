function SelectStringNotMatch()
{
   Select-String -NotMatch $args
}
function SelectStringRecursive()
{
   Get-ChildItem -recurse | sls -pattern $args
}
function GnuDiff()
{
    &'C:\Program Files\SmartGit\git\usr\bin\diff.exe' $args
}
Set-Alias grep Select-String
Set-Alias grepn SelectStringNotMatch
Set-Alias grepr SelectStringRecursive
Set-Alias wc Measure-Object
Set-Alias which Get-Command
Set-Alias gdiff GnuDiff
