function getscan, structin, up=up, down=down

if keyword_set(down) then begin
  structout=select(structin,test_status(structin,/swdown,/lwdown))
endif else begin
  structout=select(structin,1 xor test_status(structin,/swdown,/lwdown))
endelse

return, structout

end



