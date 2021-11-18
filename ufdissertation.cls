\LoadClass[12pt]{report}% The underlying class is the document class "report".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%     Edits to the base class before loading any packages     %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% We need to redefine the \@chapter to kill off the weird injected gaps between
%   and tables in their respective Lists. It's easier to do it this way than to
%   try and retroactively fix it later. 
 
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
%                    \addtocontents{lof}{\protect\addvspace{10\p@}}% <-- Gap in LoF
%                    \addtocontents{lot}{\protect\addvspace{10\p@}}% <-- Gap in LoT
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}


                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%    Support Packages and Commands    %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Generic useful configuration packages, used to configure most of the stuff below.
\RequirePackage[linktoc=all]{hyperref}% Use this to provide intra-pdf hyperlinking and better toc
\hypersetup{%               %           Setup the coloring of the links. 
%                           %           Currently the only necessary one is "colorlinks=true" and "linkcolor=blue".
    colorlinks   = true,    %           Colours links instead of ugly boxes
    urlcolor     = blue,    %           Colour for external hyperlinks
    linkcolor    = blue,    %           Colour of internal links
    citecolor    = blue     %           Colour of citations, could be ``red''
    }

\RequirePackage{etoolbox}
\RequirePackage{tabularx}
\RequirePackage[table]{xcolor}
\RequirePackage{graphicx}
\usepackage{booktabs}
\usepackage{pdflscape}
\usepackage{threeparttablex}
\RequirePackage{amsthm,amssymb,amsmath}
% \usepackage[defaultlines=4,all]{nowidow} % this one
\RequirePackage[
    format=hang,
    labelsep=period,
    justification=raggedright,
    font=singlespacing,
    singlelinecheck=false]
    {caption}
    \captionsetup[table]{skip=0pt}
%\RequirePackage{tocloft}% his is an often recommended package, but generates tons of errors/instability 
%DO NOT USE.

% \RequirePackage[plain]{algorithm}% Load the algorithm environment, which we hijack into the "Objects" environment. 
% %The "plain" option is to remove the special algorithm formating in the text.
   
\RequirePackage[largestsep]{titlesec}%  Use this for the actual header styling
%The "largestsep" option forces the gap between chapter and text to be the same as
%the gap between the chapter and section when they occur consecutively.
\RequirePackage{titletoc}%          Use this to manipulate Table of Contents styling
\RequirePackage{float}
% \RequirePackage[numbers]{natbib}

\providecommand{\realSingleSpace}{\setstretch{1}}%  They claim they want single-space, but what they actually want is "as little white space between letters as is visually acceptable".

% %%%%%%%%%%%%%%%%%%%%%%%%%
% %%% New "if" settings %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%
% 
\newif\ifdocBody%       This is to decide if we want to display the "chapter #" in the text or not.
    \docBodyfalse%      Initially we don't want "chapter #" displayed until user-content.

\newif\iftitleCapOff%   An option to override the auto-capitalization of title, for the rare time
%                           when you need to have non-capital letters in the title.
    \titleCapOfffalse%  By default titles are capitalized.

\newif\ifchapterCapOff% An option to override the auto-capitalization of chapters, for the rare time
%                           when you need to have non-capital letters in your chapters.
    \chapterCapOfffalse% By default chapters are capitalized.

\newif\ifmultipleAppendix%  Because of how LaTeX generates and saves counts for ToC
%                               We need the author to tell us if there are multiple appendicies so we can format correctly.
\multipleAppendixfalse%      We assume there will be only one appendix by default.

%%%%%%%%%%%%%%%%
%%% Margins: %%%
%%%%%%%%%%%%%%%%
%%%                         They want 1 inch all around margins, which we do with the geometry package.
\RequirePackage[
    margin=1in,%            All 4 margins need to be one inch.
    paperheight=11in,%      We want 11in tall paper.
    paperwidth=8.5in%       And 8.5in wide paper.
    ]{geometry}%            Geometry package is the easiest way to deal with margins.

%% End Margin Config.

%%%%%%%%%%%%%%%%%%%%%
%%% Font Options: %%%
%%%%%%%%%%%%%%%%%%%%%
%%%                         They want Times New Roman, or Arial. 
%%%                         Times New Roman is loaded by default, uncomment the \renewcommand below to switch it to Arial.

%\usepackage{mathptmx} % don't use this package with XeLaTeX
\renewcommand{\familydefault}{\rmdefault}
%\renewcommand{\familydefault}{\sfdefault}% Converts default to actual Arial

\raggedright%               They want a ragged right edge.
% \raggedbottom%              They also do not want references split between pages
% \widowpenalty10000%
% \clubpenalty10000%

%% End Font Setting

%%%%%%%%%%%%%%%%%%%%%%
%%% Page Numbering %%%
%%%%%%%%%%%%%%%%%%%%%%
%%%                         They want the page number in arabic numerals in the bottom middle of each page.

\RequirePackage{fancyhdr}
\pagestyle{fancy}
\renewcommand\headrulewidth{0pt}
\lhead{}\chead{}\rhead{}
\cfoot{\thepage}
\setlength{\footskip}{0.5in}%   They want the page number to be half an inch from the bottom. 
%                               This measurement is actually the distance to the text block; 
%                               So 1in margin - 0.5inch from botton = 0.5in footskip value.
%% End page numbering settings

%%%%%%%%%%%%%%%%%%%%
%%% Font Spacing %%%
%%%%%%%%%%%%%%%%%%%%

\RequirePackage[nodisplayskipstretch]{setspace}%    This will allow us to set spacing in general.
%                                                   The optional argument "nodisplayskipstretch" turns off doublespacing 
%                                                       for math display mode environments.
\doublespacing%                                     Make everything double spaced by default.
%
\AtBeginEnvironment{longtable}{\realSingleSpace}{}% Pre-append using longtables to make sure that tables are single spaced.
\preto\itemize{\par\realSingleSpace}%               Pre-append singlespace to itemize to account for 
%                                                       single space bullet point lists.
\RequirePackage{indentfirst}%                       This will make sure the first paragraph of every chapter is indented.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% Theorem environment definitions %%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% \newtheorem{theorem}{Theorem}[chapter]
%     \renewcommand{\thetheorem}{\thechapter-\arabic{theorem}}
% \newtheorem{claim}{Claim}[chapter]
%     \renewcommand{\theclaim}{\thechapter-\arabic{claim}}
% \newtheorem{lemma}{Lemma}[chapter]
%     \renewcommand{\thelemma}{\thechapter-\arabic{lemma}}


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Table Formatting %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%%% They want tables configured a certain way that makes the package "longtables" a more desirable option.
%   More specifically, from their website on text-flow:
%        Text must be continuous throughout the chapter. 
%        It is best to place all tables and figures at the end of the appropriate chapter. 
%        Avoid inserting them into the text of the chapter, unless you are able to do so 
%        without leaving blank gaps at the bottom of text pages.
%%%%%%%
\RequirePackage{longtable}
\RequirePackage{flafter}%   This package stops tables from floating backward up the text. 
%                               This stops weirdness like tables appearing before the section they are in within the source.

%%% Tables need to have their captions "sit" on the table, but figures shouldn't.


%% End table configure


%%%%%%%%%%%%%%%%%%%%%%%%
%%% Header Formatting %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%%  Some headers by default have a huge margin between the top of the chapter text and the top margin, 
%       which the grad school doesn't like. So we use the titlesec package and the \titleformat command to 
%       directly access all aspects of chapters and parts to fix spacing, formating, and style.
\titlelabel{\thetitle . \quad}

%% Define chapter's style based on if we want chapters to auto-upper-case or not.

\titleformat{\chapter}[hang]
    {\uppercase}
    {}
    {0pt}
    {\centering\realSingleSpace\ifdocBody CHAPTER \thechapter \\[-5pt] \fi}
    [\raggedright\doublespacing]
    % Look into a gap between Chapter name and text (only when it's immediately text, gap between chapter name and section header is fine. See default chapter one for example. :: Fixed via the 'largestsep' package option for titlesec.
\titleformat{\part}[hang]
    {}
    {}
    {0pt}
    {\centering\realSingleSpace\MakeUppercase}
    {\raggedright\doublespacing}


\titlespacing{\part}{0pt}{-0.7in}{0.5\baselineskip}

\titlespacing{\chapter}{0pt}{-0.7in}{0\baselineskip}

%%% Setup the formating for the actual section, subsection, and subsubsections in the text. We use the titlesec package here as it allows us to directly access the formating without having to renew the command and deal with all the code happening in the background for things like "table of contents" and pagebreaking.

\setcounter{secnumdepth}{5}
\titleformat{\section}%
    {%
        \bfseries\center\realSingleSpace%       We want \section to be bold (\bfseries), centered (\center), 
    }%                                              and single spaced (\realSingleSpace)
    {%
        \thesection%                            The section number is held in \thesection
    }%
    {1em}%                                      The space between section number and text is the standard 1em
    {}%

\titleformat{\subsection}%
    {\bfseries\raggedright\realSingleSpace}%
    {\thesubsection}%
    {1em}{}%
\titleformat{\subsubsection}
    {\bfseries\filright\realSingleSpace%     We want \subsubsection to be bold (\bfseries), left-justified (\filright), 
    }%                                              and single spaced (\realSingleSpace)
    {\thesubsubsection%                      The section number is held in \thesubsubsection
    }%
    {1em}%                                      The space between section number and text is the standard 1em
    {}

\titlespacing{\section}{0pt}{10pt}{0pt}
\titlespacing{\subsection}{0pt}{10pt}{0pt}
\titlespacing{\subsubsection}{0pt}{10pt}{0pt}
%% End of Header formating



%%%%%%%
%%% Table of Contents, List of Figures, and List of Tables fixes: They wanted hyperlinked dotted lines between the chapter name and the page number. They also want all uppercase "TABLE OF CONTENTS".
%%%%%%%

%% Fix the formatting of the various numbers.
\renewcommand*{\thefigure}{\thechapter-\arabic{figure}}%            Change the dot to a hyphen for list of figures.
% \renewcommand*{\theequation}{\thechapter-\arabic{equation}}%        Change the equation to chapter - equation#
\renewcommand*{\thetable}{\thechapter-\arabic{table}}%              Change the dot to a hyphen for list of tables.
% \renewcommand*{\thealgorithm}{\thechapter-\arabic{algorithm}}%      Change objects to be chapter - object #.



%%% Fix the titles of the "List Of ...", complete with adding the page/title at the top where applicable.
\renewcommand*\listfigurename{LIST OF FIGURES
    
    \underline{\smash{Figures}} \hfill \underline{\smash{page}}
    
    \vspace*{-0.7\baselineskip}
    }%

\renewcommand*\listtablename{LIST OF TABLES
    
    \underline{\smash{Tables}} \hfill \underline{\smash{page}}
    
    \vspace*{-0.7\baselineskip}
    }%

\renewcommand{\contentsname}{TABLE OF CONTENTS
    
    \hspace*{0pt}\hfill \underline{\smash{page}}
    
    \vspace*{-0.7\baselineskip}
    }%
    

\titlecontents{figure}[2em]
    {}
    {\hspace*{-2em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2.5em}{\contentslabel{2.5em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*\addvspace{8pt}}

\titlecontents{table}[2em]
    {}
    {\hspace*{-2em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2.5em}{\contentslabel{2.5em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*\addvspace{8pt}}
    
%%%% An attempt to do everything with titlesec and titletoc packages instead of using toclof package, to avoid package clashes.

\contentsmargin[20pt]{0pt}% This is the right-margin for the table of contents, list of figures, list of tables, and list of objects.
%                           The optional argument is the correction; the amount you shift multiline entries so that
%                               they don't overlap the page number section.
%                           The required argument is the offset from the normal right margin.

%%%%%%%
%%% Below is the formatting for all the Table of Contents hyperlinks, dotted lines, and horizontal alignment.

\titlecontents{part}[0em]
    {\mdseries}
    {}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\contentslabel{2.3em}}\hyper@linkend \\*\addvspace{6pt}}

\titlecontents{chapter}[0em]
    {\addvspace{5pt}}
    {\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2em}{\contentslabel{2em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*\addvspace{5pt}}

\titlecontents{section}[4.1em]
    {}
    {\hspace*{-2em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2em}{\contentslabel{2em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*}

\titlecontents{subsection}[6.4em]
    {}
    {\hspace*{-2.3em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2.3em}{\contentslabel{2.3em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*}

\titlecontents{subsubsection}[9.4em]
    {}
    {\hspace*{-3em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{3em}{\contentslabel{3em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%      Class Options     %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\DeclareOption{overrideChapters}{%    This option will override auto-capitalization of chapters.
    \titleformat{\chapter}[hang]%% First; auto capitalize the in-line chapter display.
        {}
        {}
        {0pt}
        {\centering\realSingleSpace\ifdocBody CHAPTER \thechapter \\ \fi}
        {\raggedright\doublespacing}
    \@latex@warning{You disabled uppercase chapters. If you didn't need to, remove `overrideChapters' from \noexpand\documentclass command to fix this.}
    \chapterCapOfftrue% Also turn off auto capitalization in the TOC.
    }

\ProcessOptions\relax%              May want to ProcessOptions at the end in order to renew commands at the end.


%%%%%%%%%%%%%%