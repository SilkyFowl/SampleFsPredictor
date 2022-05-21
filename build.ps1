$dist = "$PSScriptRoot/dist"

if(Test-Path $dist){
    Remove-Item $dist -Recurse -Force
}

dotnet publish --output dist