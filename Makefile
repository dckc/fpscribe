RUSTC=rustc  # tested with rust-nightly: 201404050405~4cf8d8c~precise

footpedal_service: libfootpedal-77ad9f83-0.0.rlib \
	footpedal_service.rs
	$(RUSTC) -O -C prefer-dynamic footpedal_service.rs -L .

# TODO: how to manage lib version?
libfootpedal-77ad9f83-0.0.rlib: footpedal.rs libtame-3d391d7b-0.0.rlib
	$(RUSTC) -O footpedal.rs -L .

libtame-3d391d7b-0.0.rlib: tame.rs
	$(RUSTC) -O tame.rs


fpscribe: fpscribe.rs
	$(RUSTC) -O -C prefer-dynamic fpscribe.rs


clean:
	rm -f *.rlib footpedal_service fpscribe
