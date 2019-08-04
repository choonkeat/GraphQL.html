elm-live: src/Templates.elm
	elm-live src/Main.elm --pushstate true

src/Templates.elm: templates $(shell find templates -iname '*.json')
	node filecontent-as-elm-methods.js templates > src/Templates.elm
