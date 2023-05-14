export const counter = (function () {
  let Counter = function () {
    this.value = 0;
  };
  Counter.prototype.increase = function () {
    this.value++;
  };
  Counter.prototype.increaseBy = function (x) {
    this.value = this.value + x;
  };
  return function () {
    return new Counter();
  };
})();

export { counter };
