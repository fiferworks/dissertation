%
% This file is copyright (C) 2003 Sam Noble. It may be modified so long
% as my name is not removed and the modifier adds his name to the file.
% Redistribution permitted.
%
% 27 Jan 2004	Sam Noble	Removed tocbibind dependency.
% 04 Dec 2001 	Sam Noble	Class file
% 03 Sep 1995	David Perkinson	Title Page
% Acknowledgements Page, David Perkinson & Sam Noble
% May 2005 Patrick Carlisle Table of contents chapter definition
% 2004-2005 Ben Salzberg (BTS) a variety of tweaks here and in the template
% 2015 Chester Ismay (CII) modified to create R Markdown template
%
% Oddities:
%
%   We *ASSUME* that any time \cleardoublepage is called
%  we actually want a blank back side with NO page number/heading
%
%   Minor bug -- seems to be a more general LaTeX thing:
%  If you use \frontmatter \mainmatter without any chapters inbetween
%  be prepared to have the page numbering messed up. Not a big deal,
%  but I'm not sure how to fix it.
%
%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{reedthesis}[2004/01/27 The Reed College Thesis Class]
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{book}}
\ProcessOptions\relax
\LoadClass{book}

% added from ufl template by ANF
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
\PassOptionsToPackage{hyphens}{url}
\RequirePackage[linktoc=all]{hyperref}% Use this to provide intra-pdf hyperlinking and better toc
\hypersetup{%               %           Setup the coloring of the links. 
%                           %           Currently the only necessary one is "colorlinks=true" and "linkcolor=blue".
    colorlinks   = true,    %           Colours links instead of ugly boxes
    urlcolor     = blue,    %           Colour for external hyperlinks
    linkcolor    = blue,    %           Colour of internal links
    citecolor    = blue     %           Colour of citations, could be ``red''
    }
\usepackage{url}
\RequirePackage{etoolbox}
\RequirePackage{tabularx}
\RequirePackage{xcolor}
\RequirePackage{amsthm,amssymb,amsmath}
\RequirePackage[
    format=hang,
    labelsep=period,
    justification=raggedright,
    font=singlespacing,
    singlelinecheck=false]
    {caption}
    \captionsetup[table]{skip=0pt}

%\RequirePackage{tocloft}%              This is an often recommended package, but generates tons of errors/instability DO NOT USE.

\RequirePackage[plain]{algorithm}%      Load the algorithm environment, which we hijack into the "Objects" environment.
%                                               The "plain" option is to remove the special algorithm formating in the text.
   
\RequirePackage[largestsep]{titlesec}%  Use this for the actual header styling
%                                           The "largestsep" option forces the gap between chapter and text to be the same as
%                                               the gap between the chapter and section when they occur consecutively.
\RequirePackage{titletoc}%          Use this to manipulate Table of Contents styling
\RequirePackage{float}

\providecommand{\realSingleSpace}{\setstretch{1}}%  They claim they want single-space, but what they actually want is 
%                                               "as little white space between letters as is visually acceptable".

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% New "if" settings %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

\newif\ifdocBody%       This is to decide if we want to display the "chapter #" in the text or not.
    \docBodyfalse%      Initially we don't want "chapter #" displayed until user-content.

\newif\iftitleCapOff%   An option to override the auto-capitalization of title, for the rare time 
%                           when you need to have non-capital letters in the title.
    \titleCapOfffalse%  By default titles are capitalized.

\newif\ifchapterCapOff% An option to override the auto-capitalization of chapters, for the rare time 
%                           when you need to have non-capital letters in your chapters.
    \chapterCapOfffalse% By default chapters are capitalized.

\newif\ifeditMode%      editMode is an option to display extra info/warnings for editing the dissertation.
%                           Also enabled the \authorRemark and \editorRemark commands.
    \editModefalse%         Default to non-edit mode. Use the package option "editMode" to turn it on.

\newif\ifdemo%          Legacy ``if" command to allow for demonstration of class features.
    \demofalse

\newif\ifmultipleAppendix%  Because of how LaTeX generates and saves counts for ToC
%                               We need the author to tell us if there are multiple appendicies so we can format correctly.
\multipleAppendixfalse%      We assume there will be only one appendix by default.

%%%%%%%
%%% Decide if we need to include a list of tables and/or a list of figures. Right now we'll do so with a manually set boolean. Would be a good idea to, instead, do some kind of check on the table/figure number and display if that value is bigger than 0.
%%%%%%%

\newif\ifhaveTables
    \haveTablesfalse%   By default we assume there are no Tables.
\newif\ifhaveFigures
    \haveFiguresfalse%  By default we assume there are no Figures.
\newif\ifhaveObjects
    \haveObjectsfalse%  By default we assume there are no Objects.

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

\RequirePackage{mathptmx}%  This is required to load and convert to times new roman
\RequirePackage{helvet}%    This is required to load Arial Font
%\renewcommand{\familydefault}{\sfdefault}% Converts default to actual Arial

\raggedright%               They want a ragged right edge.
%% End Font Setting

%%%%%%%%%%%%%%%%%%%%%%
%%% Page Numbering %%%
%%%%%%%%%%%%%%%%%%%%%%
%%%   
\RequirePackage{fancyhdr}
% Added by CII to produce nice looking code in BeginVerbatimEnvironment
\RequirePackage{fancyvrb}
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
\AtBeginEnvironment{longtable}{\realSingleSpace}{}%             Pre-append using longtables to make sure that tables are single spaced.
\preto\itemize{\par\realSingleSpace}%               Pre-append singlespace to itemize to account for 
%                                                       single space bullet point lists.
\RequirePackage{indentfirst}%                       This will make sure the first paragraph of every chapter is indented.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Formating Design    %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%
%%% Caption Configuration TBD
% NOTE: Make sure long captions don't overlap numbers in ToC :: Done.

\newcounter{figCount}%      This is an internal counter to track how many figures
\setcounter{figCount}{1}%   We will start at 1 due to how stepping it works.

\newcommand{\addFigure}[3][\Alph{figCount}]{%   Command to manually add a figure
    \parbox{#2\textwidth}{\centering #1 \\ \includegraphics[width=#2\textwidth]{#3}}
    \stepcounter{figCount}
    }

\newenvironment{multiFigure}% Environment that mimicks figure type environment,
%                               Except it doesn't float around and it resets figCount.
    {% Begin Environment Code
        \setcounter{figCount}{1}
        \minipage\textwidth
    }
    {% End Environment Code
        \endminipage
    }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Theorem environment definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newtheorem{theorem}{Theorem}[chapter]
    \renewcommand{\thetheorem}{\thechapter-\arabic{theorem}}
\newtheorem{claim}{Claim}[chapter]
    \renewcommand{\theclaim}{\thechapter-\arabic{claim}}
\newtheorem{lemma}{Lemma}[chapter]
    \renewcommand{\thelemma}{\thechapter-\arabic{lemma}}


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Title Page Formating %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Maketitle needs to be redefined to make the titlepage look how they want. 
%       Actual parameters for the title page would be nice.
%%%%%%%
\providecommand{\@docTitle}{\MakeUppercase{\@title}}%   Use this command to force uppercase titles,
%but provide an option to over ride this; using overrideTitles.
\renewcommand{\maketitle}{%
    \begin{center}
        \thispagestyle{empty}%
        \vspace*{-0.4in}\realSingleSpace{\@docTitle}%
        \vfill%
        By \\*[\baselineskip]%
        \MakeUppercase{\@author}%
        \vfill%
        A \MakeUppercase{\@thesistype} PRESENTED TO THE GRADUATE SCHOOL \\%
        OF THE UNIVERSITY OF FLORIDA IN PARTIAL FULFILLMENT \\%
        OF THE REQUIREMENTS FOR THE DEGREE OF \\%
        \MakeUppercase{\@degree} \\*[\baselineskip]%
        UNIVERSITY OF FLORIDA \\*[\baselineskip]%
        \@date%
    \end{center}
    \newpage
    }

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
\renewcommand*{\theequation}{\thechapter-\arabic{equation}}%        Change the equation to chapter - equation#
\renewcommand*{\thetable}{\thechapter-\arabic{table}}%              Change the dot to a hyphen for list of tables.
\renewcommand*{\thealgorithm}{\thechapter-\arabic{algorithm}}%      Change objects to be chapter - object #.



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
    {\hyper@linkstart{link}{\Hy@tocdestname}{\contentslabel{2.3em}}\hyper@linkend}
    {}
    {}

\titlecontents{chapter}[0em]
    {}
    {\contentslabel{2em}}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*\addvspace{8pt}}

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

%%%%%% Next is the creation of the List of Objects code.
%   Note: By default LaTeX only supports table of contents, List of tables and List of figures. The primary package to use to create a custom list is the tocloft package; but that has been depreciated and thus creates all kinds of cascading formatting problems with the other packages we use here. 
%   However, we can hijack another well maintained package that generates (among other things) a custom "List of ...". Specifically, we hijack the "algorithms" package, which gives a "list of algorithms" and an "algorithm" environment that takes captions and, in general, does everything we want. We will hijack this, then retool it to format it how we want, and finally reskin the words a little bit to make it say "object" instead of "algorithm" where needed, in the output. Since this could be a bit confusing for the author (using an 'algorithm' environment to insert an object') we conclude by defining an "addObject" command that will allow the author to add an object via the sensible "addObject" command, and that command will do all the weird formating and call all the weird environments/factors/etc to make it work, all behind the scenes.
%   Currently I don't have a good enough idea of what the formating or nature of "Objects" are, in order to actually write the "addObjects" command fully, but I have the shell defined and correct until I get additional parameters.


%%% Hijack "list of algorithms" from the already-in-use caption package, but reskin it to be a "list of objects" instead.

%%% Now setup everything to be formatted correctly as 'algorithms'.
\contentsuse{algorithm}{loa}%   Switches the definitions of things like \contentslabel to algorithms for the
%                                   titlecontents command that comes next.
\titlecontents{algorithm}[2em]% Copied from the \titlecontents for table/figure, but pointed to 'algorithms' for now.
    {}
    {\hspace*{-2em}\hyper@linkstart{link}{\Hy@tocdestname}\hspace*{2.5em}{\contentslabel{2.5em}}\hyper@linkend}
    {}
    {\hyper@linkstart{link}{\Hy@tocdestname}{\titlerule*[5pt]{.}\thecontentspage}\hyper@linkend \\*\addvspace{8pt}}
    
% Now we copy the existing "list of tables" command and patch it to work as a "list of Objects" command.
\let\listofObjects\listoftables%                                        Copy command
\patchcmd{\listofObjects}{\listtablename}{\listalgorithmname}{}{}%      Patch the three instances of \listtablename
\patchcmd{\listofObjects}{\listtablename}{\listalgorithmname}{}{}%          to all be converted to the
\patchcmd{\listofObjects}{\listtablename}{\listalgorithmname}{}{}%          \listalgorithmname.
\patchcmd{\listofObjects}{lot}{loa}{}{}%                                Get the list of objects from the .loa file.


%%% Now we do the reskin process; converting the default "algorithm" content to be "object" content.

% Rename the "algorithms" name in the \listofObjects to be "Objects". Also include the other typesetting stuff
%   like the "Object" and "page" above the list of actual objects.
\renewcommand{\listalgorithmname}{LIST OF OBJECTS
    
    \underline{\smash{Objects}} \hfill \underline{\smash{page}}
    
    \vspace*{-0.7\baselineskip}
    }%
        
\floatname{algorithm}{Object}% Change the word "Algorithm" in the in-line text to be "Object".

%% NOTE ABOUT THE FOLLOWING \addObject COMMAND::
%   I am still unclear about what exactly objects are, or how they are to be treated in the text. 
%       This command is a temporary formating command, which can be adjusted to format the "object" input by an author,
%       without the author actually needing to do much of anything, or knowing much of anything in the background.
%       At a minimum, I suspect there will need to be another required argument, where the author will supply a hyperlink.
%       As I understand it, the in-text object will be the caption, but clicking the caption/text should launch a url?

\newcommand{\addObject}[2][H]{%    This command is for the user to add an 'object' to their dissertation,
%                                   this command is intended to do all the formating for them, hiding the fact that we
%                                   hijacked the 'algorithms' list and environments to do so.
%                                   SYNTAX:: \addObject[#1]{#2}
%                                       #1 Is an optional parameter that handles placement. We use `H` by default to force
%                                           LaTeX to place it inline where the command is used, as best it can.
%                                       #2 Is the required argument, which is the caption (ie text) of the object.
    \begin{algorithm}[#1]
        \captionof{algorithm}{#2}
    \end{algorithm}
    }%

\setlength\parindent{1cm}

% Added by CII
\usepackage{ifxetex,ifluatex}
\usepackage{fixltx2e} % provides \textsubscript
\ifnum 0\ifxetex 1\fi\ifluatex 1\fi=0 % if pdftex
  \usepackage[T1]{fontenc}
  \usepackage[utf8]{inputenc}
\else % if luatex or xelatex
  \ifxetex
    %\usepackage{mathspec} %this creates a package conflict with amsmath
    \usepackage{xltxtra,xunicode}
  \else
    \usepackage{fontspec}
  \fi
  \defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}
  \newcommand{\euro}{€}
\fi
% use upquote if available, for straight quotes in verbatim environments
\IfFileExists{upquote.sty}{\usepackage{upquote}}{}
% use microtype if available
\IfFileExists{microtype.sty}{%
\usepackage{microtype}
\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts
}{}
\usepackage{color}
%\newcommand{\VerbBar}{|}
%\newcommand{\VERB}{\Verb[commandchars=\\\{\}]}
\DefineVerbatimEnvironment{verbatim}{Verbatim}{xleftmargin=-1em}
\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\{\}}
% Add ',fontsize=\small' for more characters per line
\usepackage{framed}
\definecolor{shadecolor}{RGB}{248,248,248}


\setlength{\emergencystretch}{3em}  % prevent overfull lines
\providecommand{\tightlist}{%
  \setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
% End of CII add

% This gives us rules below the headers
% \AtBeginDocument{%
% 
% % Psych majors: You do not need the following six lines, as it conflicts with apacite, so comment them out.
%   \let\oldthebibliography=\thebibliography
%   \let\endoldthebibliography=\endthebibliography
%   \renewenvironment{thebibliography}[1]{
%     \oldthebibliography{#1}
%     \addcontentsline{toc}{chapter}{\bibname}
%   }{\endoldthebibliography}
%  %%%%%% end of things for psych majors to comment out
% 
%   \let\oldtheindex=\theindex
%   \let\endoldtheindex=\endtheindex
%   \renewenvironment{theindex}{
%     \oldtheindex
%     \addcontentsline{toc}{chapter}{\indexname}
%   }{\endoldtheindex}
% }

% Stolen from book.cls and modified
\let\RToldchapter\chapter
\renewcommand{\chapter}{\if@openright\RTcleardoublepage
		    \else\clearpage\fi
		    \thispagestyle{empty}%
		    \global\@topnum\z@
		    \@afterindentfalse
		    \secdef\@chapter\@schapter}

% % Stolen from book.cls PBC 5/12/05
% % Using this to actually show "Chapter 1" in TOC instead of "1"
% \def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
%                        \if@mainmatter
%                          \refstepcounter{chapter}%
%                          \typeout{\@chapapp\space\thechapter.}%
%                          \addcontentsline{toc}{chapter}%
%                          {\@chapapp\space\thechapter:\space#1}%
%                        \else
%                          \addcontentsline{toc}{chapter}{#1}%
%                        \fi
%                     \else
%                       \addcontentsline{toc}{chapter}{#1}%
%                     \fi
%                     \chaptermark{#1}%
%                     \addtocontents{lof}{\protect\addvspace{10\p@}}%
%                     \addtocontents{lot}{\protect\addvspace{10\p@}}%
%                     \if@twocolumn
%                       \@topnewpage[\@makechapterhead{#2}]%
%                     \else
%                       \@makechapterhead{#2}%
%                       \@afterheading
%                     \fi}

\newcommand{\RTcleardoublepage}{
	\clearpage\if@twoside \ifodd\c@page\else
	\thispagestyle{empty}\hbox{}\newpage
	\if@twocolumn\hbox{}\newpage\fi\fi\fi}

\let\RToldcleardoublepage\cleardoublepage
\renewcommand{\cleardoublepage}{\RTcleardoublepage}

% adjust margins for binding  (changed 2007-04-24 tgp)
\setlength{\oddsidemargin}{.5in}
\setlength{\evensidemargin}{0in}
\setlength{\textwidth}{6.0in}
\setlength{\textheight}{9.0in}
\setlength\topmargin{0in}
\addtolength\topmargin{-\headheight}
\addtolength\topmargin{-\headsep}

%\setlength{\oddsidemargin}{.6in}
%\setlength{\evensidemargin}{0in}
%\setlength{\textwidth}{5.9in}
\addtolength\headheight{2.5pt}
%\addtolength\topmargin{-\headheight}
%\addtolength\topmargin{-\headsep}
%\addtolength\textheight{1in}
%\addtolength\textheight{\headheight}
%\addtolength\textheight{\headsep}

\def\degree#1{\gdef \@degree{#1}}
\def\@degree{\@latex@warning@no@line{No \noexpand\degree given}}
\def\institution#1{\gdef \@institution{#1}}
\def\@institution{\@latex@warning@no@line{No \noexpand\institution given}}
\def\division#1{\gdef \@division{#1}}
\def\@division{\@latex@warning@no@line{No \noexpand\division given}}
\def\department#1{\gdef \@department{#1}}
\def\@department{\@latex@warning@no@line{No \noexpand\department given}}
\def\thedivisionof#1{\gdef \@thedivisionof{#1}}
\def\@thedivisionof{The Division of}
\def\approvedforthe#1{\gdef \@approvedforthe{#1}}
\def\@approvedforthe{Division}
\def\advisor#1{\gdef \@advisor{#1}}
\def\@advisor{\@latex@warning@no@line{No \noexpand\advisor given}}
\def\altadvisor#1{\gdef \@altadvisor{#1} \@altadvisortrue}
\global\let\@altadvisor\@empty
\newif\if@altadvisor
\@altadvisorfalse
\def\thesistype#1{\gdef \@thesistype{#1}}
\def\@thesistype{\@latex@warning@no@line{No \noexpand\thesistype given}}

\renewcommand\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\normalfont
      \leaders\hbox{$\m@th \mkern \@dotsep mu\hbox{.}\mkern \@dotsep mu$}\hfill
      \nobreak\hb@xt@\@pnumwidth{\bfseries \hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}

%%% Added by CII
\newenvironment{acknowledgements}{%
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \@restonecolfalse
  \fi
  \chapter*{Acknowledgements}
  \fontsize{12}{14}\selectfont
}{\clearpage \if@restonecol\twocolumn\fi}%

\newenvironment{preface}{%
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \@restonecolfalse
  \fi
  \chapter*{Preface}
  \fontsize{12}{14}\selectfont
}{\clearpage \if@restonecol\twocolumn\fi}%
% End of CII add

\newenvironment{abstract}{%
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \@restonecolfalse
  \fi
  \chapter*{Abstract}
  \fontsize{12}{14}\selectfont
}{\clearpage \if@restonecol\twocolumn\fi}%

\newenvironment{dedication}{%
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \@restonecolfalse
  \fi
  \chapter*{Dedication}
  \fontsize{12}{14}\selectfont
}{\clearpage \if@restonecol\twocolumn\fi}%


\ifx\@pdfoutput\@undefined
  \newcommand{\RTpercent}{\@percentchar\space}
  \AtBeginDvi{\special{!\RTpercent Reed College LaTeX Thesis Class 2001/12/04 SN}}
  \AtBeginDvi{\special{rawpostscript \RTpercent Reed College LaTeX Thesis Class 2001/12/04 SN}}
\else
  \AtEndDocument{\pdfinfo{/Creator	(Reed College LaTeX Thesis Class 2001/12/04 SN)}}
\fi

% From JSS
% Added by CII
\renewcommand{\title}[1]{\def\@title{#1}}
%\newcommand{\Shorttitle}[1]{\def\@Shorttitle{#1}}
\newcommand{\Plainauthor}[1]{\def\@Plainauthor{#1}}
\newcommand{\Abstract}[1]{\def\@Abstract{#1}}
%\newcommand{\Keywords}[1]{\def\@Keywords{#1}}
%\newcommand{\Plainkeywords}[1]{\def\@Plainkeywords{#1}}
\newcommand{\Acknowledgements}[1]{\def\@Acknowledgements{#1}}
\newcommand{\Dedication}[1]{\def\@Dedication{#1}}
\newcommand{\Preface}[1]{\def\@Preface{#1}}
% End of CII add