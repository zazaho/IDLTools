;; sgn.pro
function sgn, x
  ;; Returns the sign of an array.
  result = 0*x
  pos_supp = where(x gt 0, pos_cnt)
  if (pos_cnt gt 0) then result(pos_supp) = +1
  neg_supp = where(x lt 0, neg_cnt)
  if (neg_cnt gt 0) then result(neg_supp) = -1
  return, result
end
