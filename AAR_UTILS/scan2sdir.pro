FUNCTION scan2sdir,scan
  CASE (scan) OF
    'up': return,-1
    'down': return,1
  ENDCASE
END
