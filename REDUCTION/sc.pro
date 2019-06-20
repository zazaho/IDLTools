;(SH Dec  3 1998)
function sc,inaar,insig,inniter,chauvenet=chauvenet,_extra=_extra
  
  ;; Check for conflicting options newly possible in P10!!
  ;; (SH Jan 11 2002)
  IF keyword_set(chauvenet) THEN BEGIN
      IF n_elements(inniter) NE 0 THEN BEGIN
          IF inniter GT 1 THEN BEGIN
              print,'WARNING SC: Chauvenet set with Niter>1, resetting'
              inniter = 1
          ENDIF 
      ENDIF
      IF n_elements(insig) NE 0 THEN BEGIN
          print,'WARNING SC: Chauvenet set with Sigma <> 0, resetting'
          insig = 0
      ENDIF 
  ENDIF
  
  CASE n_params() OF
    0: BEGIN 
      print,'Usage ret = sc(aar[,sig[,inerations]])'
      return,0
    END
    1: BEGIN
      return,sigclip(inaar,chauvenet=chauvenet,/nod,_extra=_extra)
    END
    2: BEGIN
      return,sigclip(inaar,sig=insig,chauvenet=chauvenet,/nod,_extra=_extra)
    END
    3: BEGIN
        return,sigclip(inaar,sig=insig,niter=inniter, $
                       chauvenet=chauvenet,/nod,_extra=_extra)
    END
  ENDCASE
END
