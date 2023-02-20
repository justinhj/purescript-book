"use strict";

export function volumeFn(a,b,c) {
  return a * b * c;
}

export const volumeArrow = a => b => c =>
  a * b * c;

const addComplex = a => b => {
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

// (-b +/- sqrt(b^2 - 4ac)) / 2a
export const quadraticRootsImpl = pairConstructor => q => {
  const disc = q.b * q.b - 4.0 * q.a * q.c;
  if(disc >= 0.0) {
    // Two real roots. (Same if discriminant is zero)
    return pairConstructor({real:
                    (-q.b + Math.sqrt(disc)) / (2 * q.a), imag: 0.0})
                   ({real:
                    (-q.b - Math.sqrt(disc)) / (2 * q.a), imag: 0.0})
  } else {
    // Two complex conjugate roots
    const real = -q.b / (2.0 * q.a);
    const imag = Math.sqrt(-disc) / (2 * q.a);
    return pairConstructor({real: real, imag: imag})({real: real, imag: -imag})
  }
}

export const quadraticRootsSetImpl = q => {
  const disc = q.b * q.b - 4.0 * q.a * q.c;
  if(disc >= 0.0) {
    // Two real roots. (Same if discriminant is zero)
    return Array.from(new Set([{real: (-q.b + Math.sqrt(disc)) / (2 * q.a), imag: 0.0},
                   {real: (-q.b - Math.sqrt(disc)) / (2 * q.a), imag: 0.0}]));
  } else {
    // Two complex conjugate roots
    const real = -q.b / (2.0 * q.a);
    const imag = Math.sqrt(-disc) / (2 * q.a);
    return Array.from(new Set([{real: real, imag: imag}, {real: real, imag: -imag}]));
  }
}

export const quadraticRootsSafeJson = q => {
  const disc = q.b * q.b - 4.0 * q.a * q.c;
  if(disc >= 0.0) {
    // Two real roots. (Same if discriminant is zero)
    return Array.from(new Set([{real: (-q.b + Math.sqrt(disc)) / (2 * q.a), imag: 0.0},
                   {real: (-q.b - Math.sqrt(disc)) / (2 * q.a), imag: 0.0}]));
  } else {
    // Two complex conjugate roots
    const real = -q.b / (2.0 * q.a);
    const imag = Math.sqrt(-disc) / (2 * q.a);
    return Array.from(new Set([{real: real, imag: imag}, {real: real, imag: -imag}]));
  }
}

export const toMaybeImpl = just => nothing => undefined$ => {
  if (undefined$ === undefined) {
    return nothing
  } else {
    return just(undefined$)
  }
}

export const valuesOfMapJson = m => {
  return Array.from(new Map(m).values());
}

