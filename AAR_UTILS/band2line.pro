FUNCTION band2line,band
  CASE (band) OF
    '1a': return,1
    '1b': return,2
    '1d': return,3
    '1e': return,4
    '2a': return,5
    '2b': return,6
    '2c': return,7
    '3a': return,9
    '3c': return,10
    '3d': return,11
    '3e': return,12
    '4': return,13
    '4a': return,20
    '4c': return,21
    '4d': return,22
    ELSE: return,0
  ENDCASE
END

    
