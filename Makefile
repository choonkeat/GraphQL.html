elm-live: src/Templates.elm
	mkdir -p public
	elm-live src/Main.elm \
            --dir public \
            --start-page index.html \
            --proxyPrefix /api \
            --proxyHost http://localhost:3000 \
            --pushstate true \
            -- --output=public/client.js

src/Templates.elm: $(shell find templates -iname '*.json')
	filecontent-as-elm-methods.js templates > src/Templates.elm
