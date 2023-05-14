export const caseNullableImpl = function(v, handle, cont, unit) {
  if(v === null) {
    return handle(v);
  }
  return cont(unit);
}
