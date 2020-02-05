param (
    [Parameter(Mandatory = $true)][string]$tag
)

stack build --copy-bins --local-bin-path release\$tag :evie
yarn run parcel build assets/elm/index.html -d release\$tag\static --public-url app
Copy-Item .\assets\config-template.json .\release\$tag\config.json -Recurse
Copy-Item .\lib\* .\release\$tag -Recurse
Copy-Item .\assets\static\css .\release\$tag\static\css -Recurse
Copy-Item .\assets\static\webfonts .\release\$tag\static\webfonts -Recurse
Copy-Item .\assets\static\lib .\release\$tag\static\lib -Recurse