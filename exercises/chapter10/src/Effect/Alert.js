"use strict";

export const alert = msg => () =>
  window.alert(msg);

export const confirm = msg => () => {
  return window.confirm(msg);
}
