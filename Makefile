elm-live:
	mkdir -p public
	elm-live src/Main.elm \
            --dir public \
            --start-page index.html \
            --proxyPrefix /api \
            --proxyHost http://localhost:3000 \
            --pushstate true \
            -- --output=public/client.js
