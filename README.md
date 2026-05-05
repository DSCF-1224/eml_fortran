# eml_fortran
Fortran implementation of the EML (Exp-Minus-Log) operator

```fortran
eml(x, y) = exp(x) - log(y)
```

[![CI](https://github.com/dscf-1224/eml_fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/dscf-1224/eml_fortran/actions/workflows/ci.yml)  
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://dscf-1224.github.io/eml_fortran/)



## Background

- Odrzywołek, A. (2026). *All elementary functions from a single operator.* (CC BY 4.0)
  - [abstract](https://arxiv.org/abs/2603.21852)
  - [SupplementaryInformation.pdf](https://arxiv.org/src/2603.21852/anc/SupplementaryInformation.pdf)
- [VA00/SymbolicRegressionPackage: Basic building blocks for brute-force and random symbolic regression methods in Mathematica](https://github.com/VA00/SymbolicRegressionPackage)


## Modules
 
### `eml_class_fortran` ([`class.f90`](src/class.f90))
 
Defines the abstract type `eml_real64_class`, which encapsulates a `complex(real64)` value.  
Provides the EML kernel `eml_operator(x, y) = exp(x) - log(y)` and `real()` / `imag()` accessors to extract the real and imaginary parts.
 
### `eml_type_fortran` ([`type.f90`](src/type.f90))

Defines the concrete type `eml_real64_type`, extending `eml_real64_class`.  
Every function is implemented purely in terms of `eml_operator` and the constant `1`.

## Requirements
 
- Fortran 2003 or later (uses `abstract` types)
- A compiler supporting `iso_fortran_env` and `ieee_arithmetic`

## License
 
MIT License — see [LICENSE](LICENSE).
 
This implementation is original work by the repository author.  
The mathematical framework (EML operator) is due to Odrzywołek (2026), [arXiv:2603.21852](https://arxiv.org/abs/2603.21852), published under CC BY 4.0.

<!-- EOF -->
