RUSTC=rustc  # tested with rust-nightly: 201404050405~4cf8d8c~precise

# TODO: how to manage lib version?
libfps_lib-77ad9f83-0.0.rlib: fps_lib.rs
	$(RUSTC) --crate-type lib fps_lib.rs 
