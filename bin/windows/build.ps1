param (
    [Parameter(Mandatory = $true)][string]$tag
)

stack build --copy-bins --local-bin-path release\$tag :evie
yarn run parcel build assets/elm/index.html -d release\$tag\static --public-url app
Copy-Item .\assets\config-template.json .\release\$tag\config.json -Recurse
Copy-Item .\lib\* .\release\$tag -Recurse
Copy-Item .\assets\lib .\release\$tag\lib -Recurse