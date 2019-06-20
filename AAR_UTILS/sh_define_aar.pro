;; To define a basic aar_structure
FUNCTION sh_define_aar,length=length,header=header,history=history
  default,header,''
  default,history,''

  return,{type   : 'SAAR'  ,$   ; structure type
          header : header  ,$   ; the header string
          history: history ,$   ; history string
          data   : replicate( {AAR_REC_STRUCT,WAVE:0.0,FLUX:0.0,STDEV:0.0,TINT:0L, $
                               DET:0L,ITK:0L,UTK:0L,RPID:[0B,0B],SPARE:[0B,0B], $
                               LINE:0L,SDIR:0L,SCNT:0L,STATUS:0L,FLAG:0L $
                              },length) } 
END
