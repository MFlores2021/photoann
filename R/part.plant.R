
part.plant <- function (p){
  if (p == 'plant') return('p') 
  if (p == 'flower') return('fl') 
  if (p == 'flower dissection') return('df') 
  if (p == 'fruit') return('b')
  if (p == 'seeds') return('s')
  if (p == 'tuber') return('t')
  if (p == 'root') return('r')
  if (p == 'sprout') return('sp')
  if (p == 'herbarium') return('hr')
  if (p == 'habitat') return('hb')
  if (p == 'foliage') return('f') 
  if (p == 'Other') return('') 
}