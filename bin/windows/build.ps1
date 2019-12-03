param (
    [Parameter(Mandatory = $true)][string]$tag
)

stack build --copy-bins --local-bin-path release\$tag :evie
yarn run parcel build assets/elm/index.html -d release\$tag\static --public-url app
cp .\assets\config-template.json .\release\$tag\config.json
cp .\lib\* .\release\$tag
cp .\assets\static\css\* .\release\$tag\static\css
cp .\assets\static\webfonts\* .\release\$tag\static\webfonts
cp .\assets\static\lib\* .\release\$tag\static\lib