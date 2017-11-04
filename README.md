# hs-screenshot
Takes a screen shot of the configured webpages and save them to disk with
the option of uploading them to [Manta](https://www.joyent.com/triton/object-storage).



    jarFile: ~/java-stuff/selenium-server-standalone-2.53.0.jar
    debug: false
    pages:
        - url: http://haskellnews.org/
          file: /tmp/hn.png
          manta: public/news/hn.png

# Required Fields
* jarFile
* pages
 * url
 * file

# Optional Fields
* debug
* manta

If you do plan to use Manta currently its configured via setting environment variables.

    $MANTA_USER
    $MANTA_URL
    $MANTA_KEY_ID

Consult the [Manta API Doc's](https://apidocs.joyent.com/manta/) for more information.

# FWIW
This is just some code I wrote to scratch an itch, not planning on making anything out of it or publishing it to hackage, etc.

Figured it might help someone who's trying to do something similar so thus its here on [github.com](github.com)
