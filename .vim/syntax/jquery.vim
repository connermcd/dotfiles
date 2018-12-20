" Vim syntax file

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'x'
endif

unlet b:current_syntax

syn match   FunctionName      /^[^:]\+:/
syn match   Return            /^RETURN\s\+/

syn region  Function          start=+^[^:]\+:+  end=+"\|$+  contains=FunctionName
syn region  javaScriptStringS          start=+'+  skip=+\\\\\|\\'+  end=+'\|$+  contains=javaScriptSpecial,@htmlPreproc,@jSelectors

syn cluster jSelectors      contains=jId,jClass,jOperators,jBasicFilters,jContentFilters,jVisibility,jChildFilters,jForms,jFormFilters
syn match   jId             contained /#[0-9A-Za-z_\-]\+/
syn match   jClass          contained /\.[0-9A-Za-z_\-]\+/
syn match   jOperators      contained /*\|>\|+\|-\|~/
syn match   jBasicFilters   contained /:\(animated\|eq\|even\|first\|focus\|gt\|header\|last\|lt\|not\|odd\)/
syn match   jChildFilters   contained /:\(first\|last\|nth\|only\)-child/
syn match   jContentFilters contained /:\(contains\|empty\|has\|parent\)/
syn match   jForms          contained /:\(button\|checkbox\|checked\|disabled\|enabled\|file\|image\|input\|password\|radio\|reset\|selected\|submit\|text\)/
syn match   jVisibility     contained /:\(hidden\|visible\)/


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_lisp_syntax_inits")
  if version < 508
    let did_lisp_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink jQuery          Constant

  HiLink jAjax           Function
  HiLink jAttributes     Function
  HiLink jCore           Function
  HiLink jCSS            Function
  HiLink jData           Function
  HiLink jDeferred       Function
  HiLink jDimensions     Function
  HiLink jEffects        Function
  HiLink jEvents         Function
  HiLink jManipulation   Function
  HiLink jMiscellaneous  Function
  HiLink jOffset         Function
  HiLink jProperties     Function
  HiLink jTraversing     Function
  HiLink jUtilities      Function

  HiLink jId             Identifier
  HiLink jClass          Constant
  HiLink jOperators      Special
  HiLink jBasicFilters   Statement
  HiLink jContentFilters Statement
  HiLink jVisibility     Statement
  HiLink jChildFilters   Statement
  HiLink jForms          Statement
  HiLink jFormFilters    Statement

  delcommand HiLink
endif


let b:current_syntax = 'x'
