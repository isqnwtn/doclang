all:
	stack run | dot -Tsvg > data/test.svg
