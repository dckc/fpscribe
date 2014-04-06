RUSTC=rustc  # tested with rust-nightly: 201404050405~4cf8d8c~precise

footpedal_service: libfootpedal-77ad9f83-0.0.rlib \
	footpedal_service.rs
	$(RUSTC) -O footpedal_service.rs -L .

# TODO: how to manage lib version?
libfootpedal-77ad9f83-0.0.rlib: footpedal.rs
	$(RUSTC) --crate-type lib -O footpedal.rs 


fpscribe: fpscribe.rs
	$(RUSTC) -O -C prefer-dynamic fpscribe.rs


clean:
	rm -f footpedal_service libfootpedal-77ad9f83-0.0.rlib \
		fpscribe
