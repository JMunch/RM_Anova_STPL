2009-02-10
Uwe Ziegenhagen, Stephan Stahlschmidt

Readme-file for the LvB-Beamer-style, made with the BEAMER package

Three versions:
- LvB_template.tex is the fully explained version without the CASE logo. Students new to LaTeX may want to start here.
- LvB_template_light.tex only provides the basic code without explanations nor the CASE logo. Anyone already familiar with LaTeX will probably prefer this version.
- LvB_CASE_template_light.tex just adds the CASE logo to the same basic code as in LvB_template_light.tex

Installation:
- Install MikTeX Complete, here everything is included
- If you cannot install the total package make sure, that all packages listed in the preamble are installed or can be installed on-the-fly.
- Choose any of the files "LvB_template.tex", "LvB_template_light.tex" or "LvB_CASE_template_light.tex" according to your need, rename it if necessary and use it as a template for your slides.
- Do not touch (unless you know what you do)
	* lvblisting.sty (includes the setup for the highlighting of (XploRe) code)
	* colordef.sty (defines several colors for listings and isestyle)
	* beamerdefs.sty (defines the layout)
- If the horizontal rules do not fit vertically, edit beamerdefs.sty
- Compile your tex file with pdflatex, compiling with LaTeX may cause errors
- Provide all graphics in JPEG, PDF or PNG format, EPS and PS is not usable by pdfLaTeX
- To convert PS to PDF, use the Acrobat Distiller or the ps2pdf command from the ghostscript utilities
- you can increase the size of the title on first page by setting 
		\def\titlescale{1.0} to e.g. \def\titlescale{1.5}
