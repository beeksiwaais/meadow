.PHONY: build run

build:
	mkdir -p priv
	cd lib && RUSTFLAGS="-C link-arg=-undefined -C link-arg=dynamic_lookup" cargo build --release
	mv lib/target/release/liblib* priv/

run:
	gleam run