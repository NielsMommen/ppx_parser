## 0.2.1
### Changed
- Fix compatibility with ppxlib 0.36 (#11)
  - Bump minimum ppxlib version to 0.36.0 (from 0.27.0)

## 0.2.0
### Added
- Bind stream to variable (#5)
- Parser from match expression (#4)
- Test workflow

### Changed
- Relax ocaml constraint to 4.8.0
- Alcotest version
- Documentation

## 0.1.1
### Changed
- Dune minimum version (2.9)
- Dependencies
- Description

## 0.1.0
Initial release

### Added
- Support `%parser` extension for `function`s to write stream parsers
- Support `... as ...` to bind stream elements
- Support `%let` extensions in `%parser` extensions to bind expressions