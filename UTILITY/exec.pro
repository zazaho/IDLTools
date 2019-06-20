PRO exec,in
  var = routine_names(/fetch,in)
  pl,var
;  foo = execute('pl,'+var)
  print,routine_names(variables)
END


; The following is a list of the keywords to routine_names, most seem to be switch keywords,
; and in many make use of 1 or 2 arguments.
;  
; argument:  list the arguments of the given routine (in the current call stack?)
; comm:  list the compiled routines (?) [default action?]
; functions: list the compiled functions.
; f_parameters: list the parameters of a function?
; f_variables: list variables in a function
; fetch: Select the level from which to fetch the  value of a variable Level: return the current
; call level
; procedures: list the compiled procedures
; p_parameters: list the parameters of a procedure?
; p_variables:  list variables in a procedure
; s:  Set variable
; s_procedures: list intrinsic procedures
; s_functions: list intrinsic functions
; unresolved: list unresolved references
; variables:  list defined variables
  
