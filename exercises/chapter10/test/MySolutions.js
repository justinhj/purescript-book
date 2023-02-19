"use strict";

export function volumeFn(a,b,c) {
  return a * b * c;
}

export const volumeArrow = a => b => c =>
  a * b * c;

export const addComplex = a => b => {
  return {
    real: a.real + b.real,
    imag: a.imag + b.imag
  }
};

export function cumulativeSumsComplex(complexArray) {
  return complexArray.reduce((acc, cur) => {
    const n = addComplex(acc[0])(cur)
    acc[1].push(n);
    return [n, acc[1]]
  }, [{real:0.0, imag:0.0}, []])[1]
}
