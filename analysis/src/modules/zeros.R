#
# Creates vector/matrix/array containing zeros with same dimensions as another vector/matrix/array.
#

zeros = function(object) {
  result = NULL

  if (is.vector(object)) {
    result = array(0, dim = length(object))
  } else {
    result = array(0, dim = dim(object))
  }

  return (result)
}
