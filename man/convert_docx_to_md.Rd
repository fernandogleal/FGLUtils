% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_docx_to_md.R
\name{convert_docx_to_md}
\alias{convert_docx_to_md}
\title{Convert DOCX File to Markdown}
\usage{
convert_docx_to_md(docx_path, output_path)
}
\arguments{
\item{docx_path}{Character string. Path to the input DOCX file.}

\item{output_path}{Character string. Path where the output Markdown file will be saved.}
}
\value{
None. The function creates a Markdown file at the specified output path.
}
\description{
This function converts a Microsoft Word (DOCX) file to Markdown format while
preserving formatting elements such as headers, tables, and images.
}
\details{
The function uses pandoc with specific options to ensure proper conversion:
\itemize{
\item Prevents automatic line wrapping
\item Uses ATX-style headers (#)
\item Extracts and saves images to a media folder
\item Preserves tabs
\item Includes document metadata
}
}
\examples{
\dontrun{
# Convert a DOCX file to Markdown
convert_docx_to_md(
  docx_path = "path/to/document.docx",
  output_path = "path/to/output.md"
)
}

}
