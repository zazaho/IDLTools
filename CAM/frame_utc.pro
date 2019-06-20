FUNCTION frame_utc,raster,frame
  
  JD_0 = 2450022.5 ;; julian date of 1-1-1995 0h00 (start of UTK)

  from = raster.from[frame]
  to   = raster.to[frame]
  
  mean_UTK = mean(raster.utk[from:to])
  mean_JD = JD_0 + mean_UTK/24.0
  return,mean_JD
END
