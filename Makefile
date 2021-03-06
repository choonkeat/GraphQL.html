elm-live: src/Templates.elm
	elm-live src/Main.elm --pushstate --after-build "./scripts/inject-init-flags-null.bash"

src/Templates.elm: templates $(shell find templates -iname '*.json')
	node scripts/filecontent-as-elm-methods.js templates > src/Templates.elm

gh-pages:
	git branch -D gh-pages
	git checkout -b gh-pages
	# cat src/Main.elm | \
	# 	sed 's/module Main exposing/module Production exposing/g' | \
	# 	sed 's/Debug./Dev./g' \
	# 	> src/Production.elm
	# elm make src/Production.elm --optimize --output=index.html
	elm make src/Main.elm --output=index.html
	./scripts/inject-init-flags-null.bash
	git add -f index.html
	git commit -av -m "make gh-pages"
	git push origin gh-pages:gh-pages --force-with-lease
	git checkout -
