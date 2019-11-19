{-# LANGUAGE QuasiQuotes #-}

module Errors where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified NeatInterpolation as NI

errorString :: Text.Text -> Text.Text -> Text.Text -> ByteString.ByteString
errorString errorHeading errroSubHeading errorMsg =
  ByteString.fromStrict $
    TextEncoding.encodeUtf8
      [NI.text|
        <!DOCTYPE html>
        <html>
            <head>
                <meta charset="UTF-8">
                <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
        
                <title>Error</title>
        
                <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css">
            </head>
            <body id="page">
              <div class="container is-fluid" id="content">
                <section data-section-id="1" data-component-id="15a7_16_01_awz" data-category="http-codes" class="section">
                    <div class="container has-text-centered">
                      <div class="columns is-centered">
                        <div class="column is-7">
                          <h1 class="title is-1" data-config-id="header">$errorHeading</h1>
                          <p class="subtitle is-3" data-config-id="subheader">$errroSubHeading</p>
                          <p data-config-id="paragraph"> $errorMsg </p>
                        </div>
                      </div><a class="button is-primary" href="/app" data-config-id="primary-action">Back to scenario list</a>
                    </div>
                  </section>
                </div>
            </body>
        </html>
  |]
