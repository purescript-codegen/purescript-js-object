export function runEffectConstr1Impl (fn, arg) {
  return new fn(arg);
};

export function runEffectConstr2Impl (fn, arg1, arg2) {
  return new fn(arg1, arg2);
};

export function runEffectConstr3Impl (fn, arg1, arg2, arg3) {
  return new fn(arg1, arg2, arg3);
};

export function runEffectConstr4Impl (fn, arg1, arg2, arg3, arg4) {
  return new fn(arg1, arg2, arg3, arg4);
};

export function runEffectConstr5Impl (fn, arg1, arg2, arg3, arg4, arg5) {
  return new fn(arg1, arg2, arg3, arg4, arg5);
};

export function runEffectConstr6Impl (fn, arg1, arg2, arg3, arg4, arg5, arg6) {
  return new fn(arg1, arg2, arg3, arg4, arg5, arg6);
};

export function unsafeRunEffectProp (mth, obj) {
  return obj[mth];
};

export function unsafeRunEffectMth0 (mth, obj) {
  return obj[mth]();
};

export function unsafeRunEffectMth1 (mth, obj, arg) {
  return obj[mth](arg);
};

export function unsafeRunEffectMth2 (mth, obj, arg1, arg2) {
  return obj[mth](arg1, arg2);
};

export function unsafeRunEffectMth3 (mth, obj, arg1, arg2, arg3) {
  return obj[mth](arg1, arg2, arg3);
};

export function unsafeRunEffectMth4 (mth, obj, arg1, arg2, arg3, arg4) {
  return obj[mth](arg1, arg2, arg3, arg4);
};

export function unsafeRunEffectMth5 (
  mth,
  obj,
  arg1,
  arg2,
  arg3,
  arg4,
  arg5
) {
  return obj[mth](arg1, arg2, arg3, arg4, arg5);
};

export function unsafeRunEffectMth6 (
  mth,
  obj,
  arg1,
  arg2,
  arg3,
  arg4,
  arg5,
  arg6
) {
  return obj[mth](arg1, arg2, arg3, arg4, arg5, arg6);
};

export function unsafeRunEffectMth7 (
  mth,
  obj,
  arg1,
  arg2,
  arg3,
  arg4,
  arg5,
  arg6,
  arg7
) {
  return obj[mth](arg1, arg2, arg3, arg4, arg5, arg6, arg7);
};
