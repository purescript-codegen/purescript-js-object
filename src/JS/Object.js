exports.unsafeRunEffectProp = function (mth, obj) {
  return obj[mth];
};

exports.unsafeRunEffectMth0 = function (mth, obj) {
  return obj[mth]();
};

exports.unsafeRunEffectMth1 = function (mth, obj, arg) {
  return obj[mth](arg);
};

exports.unsafeRunEffectMth2 = function (mth, obj, arg1, arg2) {
  return obj[mth](arg1, arg2);
};

exports.unsafeRunEffectMth3 = function (mth, obj, arg1, arg2, arg3) {
  return obj[mth](arg1, arg2, arg3);
};

exports.unsafeRunEffectMth4 = function (mth, obj, arg1, arg2, arg3, arg4) {
  return obj[mth](arg1, arg2, arg3, arg4);
};

exports.unsafeRunEffectMth5 = function (
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

exports.unsafeRunEffectMth6 = function (
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

exports.unsafeRunEffectMth7 = function (
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
