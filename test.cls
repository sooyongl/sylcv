% Define an environment for cventry
\newenvironment{cveducations}{%
  \vspace{\acvSectionContentTopSkip}
  \begin{center}
}{%
  \end{center}
}
% Define an entry of cv information
% Usage: \cventry{<position>}{<title>}{<location>}{<date>}{<description>}
\newcommand*{\cveducation}[5]{
  %  \vspace{-2.0mm}
  \setlength\tabcolsep{0pt}%
  \setlength{\extrarowheight}{0pt}%
  \begin{tabular*}{\textwidth}{@{\extracolsep{\fill}} L{\textwidth - 4.5cm} R{4.5cm}}%
  \ifempty{#2#3}{\entrypositionstyle{#1} & \entrydatestyle{#4} \\}%
    {\entrytitlestyle{#2} & \entrylocationstyle{#3} \\%
      \entrypositionstyle{#1} & \entrydatestyle{#4} \\}%
        \end{tabular*}%
      }


% Define an environment for cventry
\newenvironment{cventries}{%
  \vspace{\acvSectionContentTopSkip}
  \begin{center}
}{%
  \end{center}
}
% Define an entry of cv information
% Usage: \cventry{<position>}{<title>}{<location>}{<date>}{<description>}
\newcommand*{\cventry}[5]{
%  \vspace{-2.0mm}
  \setlength\tabcolsep{0pt}%
  \setlength{\extrarowheight}{0pt}%
  \begin{tabular*}{\textwidth}{@{\extracolsep{\fill}} L{\textwidth - 4.5cm} R{4.5cm}}%
    \ifempty{#2#3}{\entrypositionstyle{#1} & \entrydatestyle{#4} \\}%
      {\entrytitlestyle{#2} & \entrylocationstyle{#3} \\%
      \entrypositionstyle{#1} & \entrydatestyle{#4} \\
      \descriptionstyle{#5} & 
      }%
  \end{tabular*}%
}