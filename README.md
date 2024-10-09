# FGLUtils

FGLUtils is an R package containing utility functions useful for Fernando Guerra Leal (FGL). This package provides a collection of functions for various tasks including financial calculations, file operations, data formatting, and more.

## Features

The package includes functions for:

- Financial calculations (e.g., `calculate_net_salary`, `correct_inflation`, `correct_interest`)
- File operations (e.g., `cut_paste_file`, `find_files`, `merge_pdfs`)
- Data formatting (e.g., `format_number`, `format_percent`, `to_title_case_pt`)
- Data retrieval (e.g., `get_dollar_data`, `get_interest_base`)
- OCR (Optical Character Recognition) with `recognize_ocr`
- Excel operations with `to_excel`
- And more utility functions

## Installation

You can install the FGLUtils package directly from GitHub using the `devtools` package. If you don't have `devtools` installed, you can install it first:

```R
install.packages("devtools")
```

Then, you can install FGLUtils:

```R
devtools::install_github("fernandogleal/FGLUtils")
```

## Usage

After installation, you can load the package using:

```R
library(FGLUtils)
```

Then you can use any of the functions provided by the package. For example:

```R
# Format a number
format_number(1234567.89)

# Calculate net salary
calculate_net_salary(5000)

# Find files
find_files(pattern = "*.csv", path = "your/directory/path")
```

## Dependencies

This package imports several other R packages including:

bizdays, devtools, dplyr, httr, lubridate, openxlsx, pdftools, Quandl, scales, stringr, tesseract, tibble, tidyr

These will be automatically installed when you install FGLUtils.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Author

Fernando Guerra Leal (fernandoguerraleal@gmail.com)

For more information on specific functions, please refer to the package documentation.