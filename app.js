"use strict";

const COMPLEX_EPSILON = 1e-12;

function parsePolynomial(text) {
  const parts = String(text).trim().split(/[\s,，]+/).filter(Boolean);
  if (!parts.length) throw new Error("请输入至少一个多项式系数。");
  const values = parts.map((part) => {
    const value = Number(part);
    if (!Number.isFinite(value)) throw new Error(`系数“${part}”不是有效数字。`);
    return value;
  });
  while (values.length > 1 && values[0] === 0) values.shift();
  if (values[0] === 0) throw new Error("多项式首项系数不能为 0。");
  return values;
}

function trimPolynomial(values) {
  const result = values.slice();
  while (result.length > 1 && Math.abs(result[0]) < 1e-14) result.shift();
  return result.length ? result : [0];
}

function addPolynomial(a, b, sign = 1) {
  const length = Math.max(a.length, b.length);
  const result = new Array(length).fill(0);
  for (let i = 0; i < length; i += 1) {
    const ai = a[a.length - length + i] ?? 0;
    const bi = b[b.length - length + i] ?? 0;
    result[i] = ai + sign * bi;
  }
  return trimPolynomial(result);
}

function multiplyPolynomial(a, b) {
  const result = new Array(a.length + b.length - 1).fill(0);
  a.forEach((av, ai) => b.forEach((bv, bi) => { result[ai + bi] += av * bv; }));
  return trimPolynomial(result);
}

function scalePolynomial(a, factor) { return trimPolynomial(a.map((value) => value * factor)); }

function rationalAdd(a, b, sign = 1) {
  return { numerator: addPolynomial(multiplyPolynomial(a.numerator, b.denominator), multiplyPolynomial(b.numerator, a.denominator), sign), denominator: multiplyPolynomial(a.denominator, b.denominator) };
}
function rationalMultiply(a, b) { return { numerator: multiplyPolynomial(a.numerator, b.numerator), denominator: multiplyPolynomial(a.denominator, b.denominator) }; }
function rationalDivide(a, b) {
  if (b.numerator.every((value) => Math.abs(value) < 1e-14)) throw new Error("公式中不能除以零多项式。");
  return { numerator: multiplyPolynomial(a.numerator, b.denominator), denominator: multiplyPolynomial(a.denominator, b.numerator) };
}

function tokenizeFormula(text) {
  const normalized = String(text).replace(/[−–—]/g, "-").replace(/²/g, "^2").replace(/³/g, "^3").replace(/π/g, "pi").replace(/\s+/g, "");
  const tokens = [];
  let index = 0;
  while (index < normalized.length) {
    const rest = normalized.slice(index);
    const number = rest.match(/^(?:\d+(?:\.\d*)?|\.\d+)(?:e[+-]?\d+)?/i);
    if (number) { tokens.push({ type: "number", value: Number(number[0]) }); index += number[0].length; continue; }
    const identifier = rest.match(/^[a-zA-Z]+/);
    if (identifier) { tokens.push({ type: "identifier", value: identifier[0].toLowerCase() }); index += identifier[0].length; continue; }
    const symbol = normalized[index];
    if ("+-*/^()".includes(symbol)) { tokens.push({ type: symbol, value: symbol }); index += 1; continue; }
    throw new Error(`公式中无法识别字符“${symbol}”。`);
  }
  return tokens;
}

function parseTransferFunction(formula) {
  let expression = String(formula).trim().replace(/^G\s*\(\s*s\s*\)\s*=/i, "").replace(/^G\s*=/i, "");
  if (!expression) throw new Error("请输入传递函数公式，例如 25/(s^2+6s+25)。");
  const tokens = tokenizeFormula(expression); let cursor = 0;
  const constant = (value) => ({ numerator: [value], denominator: [1] });
  const variable = { numerator: [1, 0], denominator: [1] };
  const peek = () => tokens[cursor];
  const startsFactor = (token) => token && (token.type === "number" || token.type === "identifier" || token.type === "(");
  const parseExpression = () => {
    let value = parseTerm();
    while (peek()?.type === "+" || peek()?.type === "-") { const operator = tokens[cursor++].type; value = rationalAdd(value, parseTerm(), operator === "+" ? 1 : -1); }
    return value;
  };
  const parseTerm = () => {
    let value = parsePower();
    while (peek()?.type === "*" || peek()?.type === "/" || startsFactor(peek())) {
      const operator = peek()?.type === "*" || peek()?.type === "/" ? tokens[cursor++].type : "*";
      const right = parsePower(); value = operator === "/" ? rationalDivide(value, right) : rationalMultiply(value, right);
    }
    return value;
  };
  const parsePower = () => {
    let value = parseUnary();
    if (peek()?.type === "^") {
      cursor += 1; const exponent = parseUnary();
      if (exponent.denominator.length !== 1 || exponent.numerator.length !== 1 || !Number.isInteger(exponent.numerator[0])) throw new Error("公式的幂次必须是整数。");
      const power = exponent.numerator[0]; if (power < 0 || power > 12) throw new Error("公式幂次请使用 0 到 12 之间的整数。");
      let result = constant(1); for (let i = 0; i < power; i += 1) result = rationalMultiply(result, value); value = result;
    }
    return value;
  };
  const parseUnary = () => {
    if (peek()?.type === "+") { cursor += 1; return parseUnary(); }
    if (peek()?.type === "-") { cursor += 1; const value = parseUnary(); return { numerator: scalePolynomial(value.numerator, -1), denominator: value.denominator }; }
    return parsePrimary();
  };
  const parsePrimary = () => {
    const token = tokens[cursor++];
    if (!token) throw new Error("公式不完整，请检查括号和运算符。");
    if (token.type === "number") return constant(token.value);
    if (token.type === "identifier") {
      if (token.value === "s") return variable;
      if (token.value === "pi") return constant(Math.PI);
      throw new Error(`暂不支持变量“${token.value}”，请使用 s。`);
    }
    if (token.type === "(") {
      const value = parseExpression();
      if (peek()?.type !== ")") throw new Error("公式括号不匹配。");
      cursor += 1; return value;
    }
    throw new Error("公式中存在无法解析的项。");
  };
  const result = parseExpression();
  if (cursor !== tokens.length) throw new Error(`公式中“${tokens[cursor].value}”附近无法继续解析。`);
  const scale = result.denominator[0] || 1;
  return { numerator: scalePolynomial(result.numerator, 1 / scale), denominator: scalePolynomial(result.denominator, 1 / scale) };
}

function parseNumberList(text, label) {
  const values = String(text).trim().split(/[\s,，;；]+/).filter(Boolean).map(Number);
  if (!values.length || values.some((value) => !Number.isFinite(value))) throw new Error(`${label}包含无效数字。`);
  return values;
}

function addComplex(a, b) { return { re: a.re + b.re, im: a.im + b.im }; }
function subComplex(a, b) { return { re: a.re - b.re, im: a.im - b.im }; }
function mulComplex(a, b) { return { re: a.re * b.re - a.im * b.im, im: a.re * b.im + a.im * b.re }; }
function divComplex(a, b) {
  const d = b.re * b.re + b.im * b.im;
  if (d < COMPLEX_EPSILON) return { re: 1e12, im: 1e12 };
  return { re: (a.re * b.re + a.im * b.im) / d, im: (a.im * b.re - a.re * b.im) / d };
}
function absComplex(a) { return Math.hypot(a.re, a.im); }
function evalPolynomial(coefficients, z) {
  let value = { re: coefficients[0], im: 0 };
  for (let i = 1; i < coefficients.length; i += 1) value = addComplex(mulComplex(value, z), { re: coefficients[i], im: 0 });
  return value;
}

function polynomialRoots(coefficients) {
  const degree = coefficients.length - 1;
  if (degree <= 0) return [];
  const normalized = coefficients.map((value) => value / coefficients[0]);
  if (degree === 1) return [{ re: -normalized[1], im: 0 }];
  const radius = 1 + Math.max(...normalized.slice(1).map(Math.abs));
  let roots = Array.from({ length: degree }, (_, i) => {
    const angle = (2 * Math.PI * (i + 0.173) / degree) + 0.13;
    return { re: radius * Math.cos(angle), im: radius * Math.sin(angle) };
  });
  for (let iteration = 0; iteration < 1400; iteration += 1) {
    let maxShift = 0;
    const next = roots.map((root, i) => {
      let divisor = { re: 1, im: 0 };
      for (let j = 0; j < roots.length; j += 1) {
        if (i === j) continue;
        let difference = subComplex(root, roots[j]);
        if (absComplex(difference) < COMPLEX_EPSILON) difference = addComplex(difference, { re: 1e-7 * (i + 1), im: 1e-7 * (j + 1) });
        divisor = mulComplex(divisor, difference);
      }
      const shift = divComplex(evalPolynomial(normalized, root), divisor);
      if (!Number.isFinite(shift.re) || !Number.isFinite(shift.im)) return root;
      maxShift = Math.max(maxShift, absComplex(shift));
      return subComplex(root, shift);
    });
    roots = next;
    if (maxShift < 1e-9) break;
  }
  return roots.map((root) => Math.abs(root.im) < 1e-9 ? { re: root.re, im: 0 } : root);
}

function validateModel(numerator, denominator) {
  if (numerator.length >= denominator.length) throw new Error("当前版本要求真有理传递函数：分子次数必须小于分母次数。");
  const divisor = denominator[0];
  return { numerator: numerator.map((x) => x / divisor), denominator: denominator.map((x) => x / divisor) };
}

function derivative(state, denominator, numerator) {
  const n = denominator.length - 1;
  const result = new Array(n);
  for (let i = 0; i < n - 1; i += 1) result[i] = state[i + 1];
  let last = 1;
  for (let i = 0; i < n; i += 1) last -= denominator[n - i] * state[i];
  result[n - 1] = last;
  let output = 0;
  const padded = new Array(n).fill(0);
  for (let i = 0; i < numerator.length; i += 1) padded[n - numerator.length + i] = numerator[i];
  for (let i = 0; i < n; i += 1) output += padded[n - 1 - i] * state[i];
  return { state: result, output };
}

function simulateStep(numerator, denominator, duration, sampleCount = 1800) {
  const n = denominator.length - 1;
  const dt = duration / sampleCount;
  let state = new Array(n).fill(0);
  const samples = [];
  const padded = new Array(n).fill(0);
  for (let i = 0; i < numerator.length; i += 1) padded[n - numerator.length + i] = numerator[i];
  const outputOf = (x) => x.reduce((sum, item, i) => sum + padded[n - 1 - i] * item, 0);
  for (let step = 0; step <= sampleCount; step += 1) {
    samples.push({ t: step * dt, y: outputOf(state) });
    if (step === sampleCount) break;
    const f1 = derivative(state, denominator, numerator).state;
    const s2 = state.map((v, i) => v + f1[i] * dt / 2);
    const f2 = derivative(s2, denominator, numerator).state;
    const s3 = state.map((v, i) => v + f2[i] * dt / 2);
    const f3 = derivative(s3, denominator, numerator).state;
    const s4 = state.map((v, i) => v + f3[i] * dt);
    const f4 = derivative(s4, denominator, numerator).state;
    state = state.map((v, i) => v + dt * (f1[i] + 2 * f2[i] + 2 * f3[i] + f4[i]) / 6);
    if (state.some((value) => !Number.isFinite(value) || Math.abs(value) > 1e12)) {
      return { samples, overflow: true };
    }
  }
  return { samples, overflow: false };
}

function calculateMetrics(samples, finalValue) {
  if (!Number.isFinite(finalValue)) return { valid: false };
  const first = samples[0]?.y ?? 0;
  const delta = finalValue - first;
  if (Math.abs(delta) < 1e-10) return { valid: false, finalValue };
  const direction = Math.sign(delta);
  const peak = Math.max(...samples.map((sample) => (sample.y - first) * direction));
  const overshoot = Math.max(0, (peak - Math.abs(delta)) / Math.abs(delta) * 100);
  const crossingTime = (fraction) => {
    const target = first + delta * fraction;
    for (let i = 1; i < samples.length; i += 1) {
      const before = (samples[i - 1].y - target) * direction;
      const after = (samples[i].y - target) * direction;
      if (before <= 0 && after >= 0) {
        const span = samples[i].y - samples[i - 1].y;
        const ratio = Math.abs(span) < 1e-14 ? 0 : (target - samples[i - 1].y) / span;
        return samples[i - 1].t + (samples[i].t - samples[i - 1].t) * ratio;
      }
    }
    return null;
  };
  const lower = finalValue - Math.abs(delta) * 0.02;
  const upper = finalValue + Math.abs(delta) * 0.02;
  let settling = null;
  for (let i = samples.length - 1; i >= 0; i -= 1) {
    if (samples[i].y < lower || samples[i].y > upper) {
      settling = i + 1 < samples.length ? samples[i + 1].t : null;
      break;
    }
  }
  if (settling === null && (samples[0].y < lower || samples[0].y > upper)) settling = 0;
  return { valid: true, finalValue, overshoot, riseTime: (() => {
    const t10 = crossingTime(0.1); const t90 = crossingTime(0.9);
    return t10 === null || t90 === null ? null : t90 - t10;
  })(), settlingTime: settling, peakValue: finalValue + direction * Math.max(0, peak - Math.abs(delta)) };
}

function frequencyResponse(numerator, denominator, omega) {
  let n = { re: 0, im: 0 };
  let d = { re: 0, im: 0 };
  const z = { re: 0, im: omega };
  for (const coefficient of numerator) n = addComplex(mulComplex(n, z), { re: coefficient, im: 0 });
  for (const coefficient of denominator) d = addComplex(mulComplex(d, z), { re: coefficient, im: 0 });
  return divComplex(n, d);
}

function formatPolynomial(coefficients) {
  const degree = coefficients.length - 1;
  const superscripts = { 2: "²", 3: "³", 4: "⁴", 5: "⁵", 6: "⁶", 7: "⁷", 8: "⁸" };
  return coefficients.map((coefficient, index) => {
    const power = degree - index;
    const magnitude = Math.abs(coefficient);
    if (magnitude === 0) return null;
    const variable = power === 0 ? "" : power === 1 ? "s" : `s${superscripts[power] || `^${power}`}`;
    const value = power > 0 && magnitude === 1 ? "" : String(magnitude);
    return { sign: coefficient < 0 ? "−" : "+", text: `${value}${variable}` };
  }).filter(Boolean).map((term, index) => `${index === 0 ? (term.sign === "−" ? "−" : "") : ` ${term.sign} `}${term.text}`).join("") || "0";
}

function formatComplex(value, digits) {
  if (!value || !Number.isFinite(value.re) || !Number.isFinite(value.im)) return "—";
  const places = digits === undefined ? 3 : digits;
  const re = Math.abs(value.re) < 1e-8 ? 0 : value.re;
  const im = Math.abs(value.im) < 1e-8 ? 0 : value.im;
  const fmt = (number) => Number(number.toFixed(places)).toString();
  if (im === 0) return fmt(re);
  if (re === 0) return fmt(im) + "j";
  return fmt(re) + (im >= 0 ? " + " : " − ") + fmt(Math.abs(im)) + "j";
}

function polynomialDerivative(coefficients) {
  const degree = coefficients.length - 1;
  return degree > 0 ? coefficients.slice(0, -1).map((value, index) => value * (degree - index)) : [0];
}

function modelCharacteristics(model) {
  const poles = polynomialRoots(model.denominator);
  const zeros = polynomialRoots(model.numerator);
  const type = poles.filter((pole) => Math.abs(pole.re) < 1e-7 && Math.abs(pole.im) < 1e-7).length;
  const relativeOrder = model.denominator.length - model.numerator.length;
  const gain = model.numerator[0] / model.denominator[0];
  const corners = [...zeros.map((root) => ({ omega: absComplex(root), kind: "零点", root })), ...poles.map((root) => ({ omega: absComplex(root), kind: "极点", root }))]
    .filter((item) => item.omega > 1e-7 && Number.isFinite(item.omega)).sort((a, b) => a.omega - b.omega);
  return { poles, zeros, type, relativeOrder, gain, corners };
}

function asymptoticMagnitudeDb(model, roots, omega) {
  const safeOmega = Math.max(omega, 1e-12);
  let ratio = Math.abs(model.numerator[0] / model.denominator[0]);
  roots.zeros.forEach((root) => { const corner = absComplex(root); ratio *= corner < 1e-8 || safeOmega >= corner ? safeOmega : corner; });
  roots.poles.forEach((root) => { const corner = absComplex(root); ratio /= corner < 1e-8 || safeOmega >= corner ? safeOmega : corner; });
  return 20 * Math.log10(Math.max(ratio, 1e-12));
}

function asymptoticSlopeDbDecade(roots, omega) {
  const activeZeros = roots.zeros.filter((root) => absComplex(root) < 1e-8 || omega >= absComplex(root)).length;
  const activePoles = roots.poles.filter((root) => absComplex(root) < 1e-8 || omega >= absComplex(root)).length;
  return 20 * (activeZeros - activePoles);
}

function dedupeNumbers(values, tolerance) {
  const threshold = tolerance || 1e-5;
  return values.filter((value) => Number.isFinite(value) && value > 0).sort((a, b) => a - b).filter((value, index, sorted) => index === 0 || Math.abs(Math.log(value / sorted[index - 1])) > threshold);
}

function estimateCrossover(data, field, target) {
  const level = target || 0;
  for (let i = 1; i < data.length; i += 1) {
    const before = data[i - 1][field] - level;
    const after = data[i][field] - level;
    if (before === 0) return data[i - 1].omega;
    if (before * after <= 0) {
      const ratio = Math.abs(after - before) < 1e-12 ? 0 : -before / (after - before);
      return data[i - 1].omega * Math.pow(data[i].omega / data[i - 1].omega, ratio);
    }
  }
  return null;
}

function estimateCrossovers(data, field, target) {
  const crossings = [];
  const add = (omega) => {
    if (Number.isFinite(omega) && !crossings.some((item) => Math.abs(Math.log(omega / item)) < 1e-7)) crossings.push(omega);
  };
  for (let i = 1; i < data.length; i += 1) {
    const before = data[i - 1][field] - target;
    const after = data[i][field] - target;
    if (Math.abs(before) < 1e-10) add(data[i - 1].omega);
    if (before * after < 0) {
      const ratio = -before / (after - before);
      add(data[i - 1].omega * Math.pow(data[i].omega / data[i - 1].omega, ratio));
    }
    if (i === data.length - 1 && Math.abs(after) < 1e-10) add(data[i].omega);
  }
  return crossings;
}

function interpolateFrequencyField(data, omega, field) {
  if (!data.length || omega < data[0].omega || omega > data[data.length - 1].omega) return null;
  for (let i = 1; i < data.length; i += 1) {
    if (omega <= data[i].omega) {
      const fraction = Math.log(omega / data[i - 1].omega) / Math.log(data[i].omega / data[i - 1].omega);
      return data[i - 1][field] + (data[i][field] - data[i - 1][field]) * fraction;
    }
  }
  return data.at(-1)[field];
}

function evaluateQuery(data, mode, value) {
  if (!Number.isFinite(value)) return "请输入有效数值。";
  if (mode === "phase-at-frequency") {
    if (value <= 0) return "频率必须大于 0。";
    if (value < data[0].omega || value > data.at(-1).omega) return "频率超出当前图的范围。";
    const phase = interpolateFrequencyField(data, value, "phase");
    const magnitude = interpolateFrequencyField(data, value, "magnitude");
    return "ω = " + value.toPrecision(5) + " rad/s（f = " + (value / (2 * Math.PI)).toPrecision(5) + " Hz）时，相位约为 " + phase.toFixed(2) + "°，幅值 " + magnitude.toFixed(2) + " dB。";
  }
  const crossings = [];
  for (let i = 1; i < data.length; i += 1) {
    const before = data[i - 1].phase - value;
    const after = data[i].phase - value;
    if (before * after <= 0) {
      const ratio = Math.abs(after - before) < 1e-12 ? 0 : -before / (after - before);
      crossings.push(data[i - 1].omega * Math.pow(data[i].omega / data[i - 1].omega, ratio));
    }
  }
  return crossings.length ? "相位 " + value.toFixed(2) + "° 对应频率约 " + crossings.map((omega) => omega.toPrecision(5) + " rad/s（" + (omega / (2 * Math.PI)).toPrecision(5) + " Hz）").join("、") + "。" : "在当前频率范围内没有穿过 " + value.toFixed(2) + "°。";
}

function findBreakawayPoints(model, roots) {
  const q = addPolynomial(
    multiplyPolynomial(polynomialDerivative(model.denominator), model.numerator),
    multiplyPolynomial(model.denominator, polynomialDerivative(model.numerator)),
    -1,
  );
  return polynomialRoots(q).filter((point) => Math.abs(point.im) < 1e-5).map((point) => point.re).filter((s) => {
    if (Math.abs(s) < 1e-8) return false;
    const numerator = evalPolynomial(model.numerator, { re: s, im: 0 }).re;
    const denominator = evalPolynomial(model.denominator, { re: s, im: 0 }).re;
    const gain = Math.abs(numerator) < 1e-10 ? -Infinity : -denominator / numerator;
    const polesToRight = roots.poles.filter((pole) => Math.abs(pole.im) < 1e-6 && pole.re > s).length;
    return gain > 0 && polesToRight % 2 === 1;
  }).sort((a, b) => a - b);
}

function findImaginaryCrossings(model) {
  const roots = modelCharacteristics(model);
  const maxFrequency = Math.max(10, ...roots.poles.map(absComplex), ...roots.zeros.map(absComplex)) * 100;
  const samples = 1800;
  const candidates = [];
  let previous = null;
  for (let i = 0; i <= samples; i += 1) {
    const omega = i === 0 ? 0 : Math.pow(10, -4 + 4 * i / samples) * maxFrequency / 10;
    const numerator = evalPolynomial(model.numerator, { re: 0, im: omega });
    const denominator = evalPolynomial(model.denominator, { re: 0, im: omega });
    const ratio = divComplex(denominator, numerator);
    const point = { omega, gain: -ratio.re, residual: -ratio.im };
    if (previous && point.gain > 0 && previous.gain > 0 && point.residual * previous.residual <= 0) {
      const weight = Math.abs(previous.residual) / (Math.abs(previous.residual) + Math.abs(point.residual) || 1);
      const crossingOmega = previous.omega + (omega - previous.omega) * weight;
      const crossingGain = previous.gain + (point.gain - previous.gain) * weight;
      if (!candidates.some((item) => Math.abs(item.omega - crossingOmega) / Math.max(crossingOmega, 1) < 0.02)) candidates.push({ omega: crossingOmega, gain: crossingGain });
    }
    previous = point;
  }
  return candidates;
}

function routhTable(coefficients) {
  const normalized = trimPolynomial(coefficients);
  const order = normalized.length - 1;
  const columns = Math.ceil((order + 1) / 2);
  const table = Array.from({ length: order + 1 }, () => new Array(columns).fill(0));
  normalized.forEach((value, index) => {
    const row = index % 2;
    const column = Math.floor(index / 2);
    table[row][column] = value;
  });
  const epsilon = 1e-9;
  const specialRows = [];
  for (let row = 2; row <= order; row += 1) {
    if (table[row - 1].every((value) => Math.abs(value) < epsilon)) {
      specialRows.push(row - 1);
      const auxiliaryOrder = order - (row - 1) + 1;
      const auxiliary = table[row - 2];
      for (let column = 0; column < columns; column += 1) {
        const power = auxiliaryOrder - 2 * column;
        table[row - 1][column] = power > 0 ? auxiliary[column] * power : 0;
      }
    }
    if (Math.abs(table[row - 1][0]) < epsilon) table[row - 1][0] = epsilon;
    for (let column = 0; column < columns - 1; column += 1) {
      const numerator = table[row - 1][0] * table[row - 2][column + 1] - table[row - 2][0] * table[row - 1][column + 1];
      table[row][column] = numerator / table[row - 1][0];
    }
  }
  const firstColumn = table.map((row) => row[0]);
  const signs = firstColumn.map((value) => Math.sign(value));
  let signChanges = 0;
  for (let i = 1; i < signs.length; i += 1) if (signs[i] && signs[i - 1] && signs[i] !== signs[i - 1]) signChanges += 1;
  return { table, order, firstColumn, signChanges, specialRows };
}

function closedLoopPolynomial(model, gain) {
  const length = Math.max(model.denominator.length, model.numerator.length);
  const denominator = new Array(length - model.denominator.length).fill(0).concat(model.denominator);
  const numerator = new Array(length - model.numerator.length).fill(0).concat(model.numerator);
  return addPolynomial(denominator, scalePolynomial(numerator, gain));
}

function nyquistData(model, gain, samples) {
  const count = samples || 400;
  const characteristics = modelCharacteristics(model);
  const imaginaryAxisPoles = characteristics.poles.filter((pole) => Math.abs(pole.re) < 1e-7);
  const maxFrequency = Math.max(100, ...characteristics.poles.map(absComplex), ...characteristics.zeros.map(absComplex)) * 100;
  const positive = Array.from({ length: count }, (_, index) => {
    const omega = Math.pow(10, -5 + 10 * index / (count - 1)) * maxFrequency / 10;
    const response = frequencyResponse(model.numerator, model.denominator, omega);
    return { omega, x: gain * response.re, y: gain * response.im };
  });
  const dc = Math.abs(evalPolynomial(model.denominator, { re: 0, im: 0 }).re) < 1e-10 ? null : frequencyResponse(model.numerator, model.denominator, 0);
  const negative = positive.slice().reverse().map((point) => ({ omega: -point.omega, x: point.x, y: -point.y }));
  return { negative, dc: dc ? { omega: 0, x: gain * dc.re, y: gain * dc.im } : null, positive, maxFrequency, imaginaryAxisPole: imaginaryAxisPoles.length > 0, imaginaryAxisPoles };
}

function nyquistWinding(data, critical) {
  const path = [...data.negative, ...(data.dc ? [data.dc] : []), ...data.positive].map((point) => ({ re: point.x - critical, im: point.y }));
  let angleSum = 0;
  for (let i = 1; i < path.length; i += 1) {
    const a = path[i - 1]; const b = path[i];
    const cross = a.re * b.im - a.im * b.re;
    const dot = a.re * b.re + a.im * b.im;
    angleSum += Math.atan2(cross, dot);
  }
  return Math.round(angleSum / (2 * Math.PI));
}

const lineChartState = new WeakMap();

function makeSvgLineChart(container, series, options) {
  const svg = container.querySelector("svg");
  const width = Math.max(320, Math.round(container.clientWidth || 700));
  const height = Math.max(160, Math.round(container.clientHeight || 260));
  const left = 54; const right = 17; const top = 13; const bottom = 34;
  const plotWidth = width - left - right; const plotHeight = height - top - bottom;
  const xMin = options.viewDomain?.xMin ?? options.xMin ?? Math.min(...series.flatMap((s) => s.data.map((p) => p.x)));
  const xMax = options.viewDomain?.xMax ?? options.xMax ?? Math.max(...series.flatMap((s) => s.data.map((p) => p.x)));
  let yMin = options.viewDomain?.yMin ?? options.yMin ?? Math.min(...series.flatMap((s) => s.data.map((p) => p.y)));
  let yMax = options.viewDomain?.yMax ?? options.yMax ?? Math.max(...series.flatMap((s) => s.data.map((p) => p.y)));
  if (!Number.isFinite(yMin) || !Number.isFinite(yMax)) { yMin = -1; yMax = 1; }
  if (Math.abs(yMax - yMin) < 1e-8) { yMin -= 1; yMax += 1; }
  const pad = options.viewDomain || options.padding === false ? 0 : (yMax - yMin) * 0.08;
  yMin -= pad; yMax += pad;
  const sx = (x) => left + (x - xMin) / (xMax - xMin || 1) * plotWidth;
  const sy = (y) => top + (yMax - y) / (yMax - yMin) * plotHeight;
  const xTicks = options.xTicks ?? 5; const yTicks = options.yTicks ?? 4;
  const xLabel = options.xLabel || "t (s)"; const yLabel = options.yLabel || "y(t)";
  const fmt = options.formatY || ((value) => Number(value.toPrecision(3)).toString());
  const fmtX = options.formatX || ((value) => Number(value.toPrecision(3)).toString());
  let markup = "";
  for (let i = 0; i <= yTicks; i += 1) {
    const value = yMin + (yMax - yMin) * i / yTicks; const y = sy(value);
    markup += `<line class="chart-grid" x1="${left}" y1="${y}" x2="${width-right}" y2="${y}"/><text class="chart-tick" x="${left-8}" y="${y+3}" text-anchor="end">${fmt(value)}</text>`;
  }
  for (let i = 0; i <= xTicks; i += 1) {
    const value = xMin + (xMax - xMin) * i / xTicks; const x = sx(value);
    markup += `<line class="chart-grid" x1="${x}" y1="${top}" x2="${x}" y2="${height-bottom}"/><text class="chart-tick" x="${x}" y="${height-13}" text-anchor="middle">${fmtX(value)}</text>`;
  }
  markup += `<line class="chart-axis" x1="${left}" y1="${top}" x2="${left}" y2="${height-bottom}"/><line class="chart-axis" x1="${left}" y1="${height-bottom}" x2="${width-right}" y2="${height-bottom}"/>`;
  if (options.reference !== undefined && options.reference >= yMin && options.reference <= yMax) {
    const y = sy(options.reference);
    markup += `<line class="chart-reference" x1="${left}" y1="${y}" x2="${width-right}" y2="${y}"/><text class="chart-reference-label" x="${width-right-3}" y="${y-5}" text-anchor="end">最终值 ${fmt(options.reference)}</text>`;
  }
  (options.verticals || []).forEach((line) => {
    if (line.x < xMin || line.x > xMax) return;
    const x = sx(line.x);
    markup += '<line class="chart-annotation-line ' + (line.className || "") + '" x1="' + x + '" y1="' + top + '" x2="' + x + '" y2="' + (height - bottom) + '"/>';
    if (line.label) markup += '<text class="chart-annotation-label" x="' + (x + 4) + '" y="' + (top + 12 + (line.offset || 0)) + '">' + line.label + '</text>';
  });
  series.forEach((item) => {
    const data = item.data.filter((point) => Number.isFinite(point.x) && Number.isFinite(point.y));
    const d = data.map((point, i) => `${i === 0 ? "M" : "L"}${sx(point.x).toFixed(2)},${sy(point.y).toFixed(2)}`).join(" ");
    if (item.fill && d) markup += `<path class="plot-fill" d="${d} L${sx(data[data.length-1].x)},${sy(options.fillBase ?? yMin)} L${sx(data[0].x)},${sy(options.fillBase ?? yMin)} Z"/>`;
    markup += `<path class="${item.className || "chart-data-line"}" d="${d}"/>`;
  });
  (options.points || []).forEach((point) => {
    if (point.x < xMin || point.x > xMax || point.y < yMin || point.y > yMax) return;
    const x = sx(point.x); const y = sy(point.y);
    markup += '<circle class="chart-annotation-point ' + (point.className || "") + '" cx="' + x + '" cy="' + y + '" r="3.5"/><text class="chart-annotation-label" x="' + (x + 5) + '" y="' + (y - 7) + '">' + (point.label || "") + '</text>';
  });
  markup += `<text class="chart-axis-title" x="${left + plotWidth/2}" y="${height-1}" text-anchor="middle">${xLabel}</text><text class="chart-axis-title" transform="translate(13 ${top+plotHeight/2}) rotate(-90)" text-anchor="middle">${yLabel}</text>`;
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  svg.innerHTML = markup;
  attachChartInteractions(container, series, options, { width, height, left, right, top, bottom, plotWidth, plotHeight, xMin, xMax, yMin, yMax });
}

function attachChartInteractions(container, series, options, layout) {
  const svg = container.querySelector("svg");
  let state = lineChartState.get(container);
  if (!state) { state = { zoom: null, pinned: false }; lineChartState.set(container, state); }
  state.series = series; state.options = options; state.layout = layout;
  state.domain = { xMin: layout.xMin, xMax: layout.xMax, yMin: layout.yMin, yMax: layout.yMax };
  if (!options.viewDomain) {
    state.baseOptions = options;
    state.pinned = false;
  }
  let readout = container.nextElementSibling && container.nextElementSibling.classList.contains("chart-readout") ? container.nextElementSibling : null;
  if (!readout) {
    readout = document.createElement("div"); readout.className = "chart-readout"; readout.setAttribute("aria-live", "polite");
    container.insertAdjacentElement("afterend", readout);
  }
  state.readout = readout;
  const hint = () => state.options.readoutHint || "滚轮缩放 · 双击恢复 · 鼠标移到曲线上读取坐标";
  if (!state.listenersAttached) {
    const coordinate = (event) => {
      const rect = svg.getBoundingClientRect(); const current = state.layout;
      return { x: (event.clientX - rect.left) * current.width / rect.width, y: (event.clientY - rect.top) * current.height / rect.height };
    };
    const inspect = (event) => {
      const pos = coordinate(event); const current = state.layout; const domain = state.domain;
      if (pos.x < current.left || pos.x > current.width - current.right || pos.y < current.top || pos.y > current.height - current.bottom) return false;
      let nearest = null; let minDistance = Infinity;
      state.series.forEach((item) => item.data.forEach((point) => {
        if (!Number.isFinite(point.x) || !Number.isFinite(point.y)) return;
        const px = current.left + (point.x - domain.xMin) / (domain.xMax - domain.xMin || 1) * current.plotWidth;
        const py = current.top + (domain.yMax - point.y) / (domain.yMax - domain.yMin || 1) * current.plotHeight;
        const dx = (px - pos.x) / current.plotWidth; const dy = (py - pos.y) / current.plotHeight;
        const distance = dx * dx + dy * dy;
        if (distance < minDistance) { minDistance = distance; nearest = { point, px, py, name: item.name || item.label || "" }; }
      }));
      if (!nearest) return false;
      const pxToX = domain.xMin + (nearest.px - current.left) / current.plotWidth * (domain.xMax - domain.xMin);
      const currentOptions = state.options;
      const xText = currentOptions.formatReadoutX ? currentOptions.formatReadoutX(nearest.point.x) : (currentOptions.formatX ? currentOptions.formatX(nearest.point.x) : Number(nearest.point.x.toPrecision(5)).toString());
      const yText = currentOptions.formatReadoutY ? currentOptions.formatReadoutY(nearest.point.y) : (currentOptions.formatY ? currentOptions.formatY(nearest.point.y) : Number(nearest.point.y.toPrecision(5)).toString());
      state.readout.textContent = (nearest.name ? nearest.name + " · " : "") + currentOptions.xLabel + " = " + xText + " · " + currentOptions.yLabel + " = " + yText;
      let crosshair = svg.querySelector(".chart-crosshair");
      if (!crosshair) {
        crosshair = document.createElementNS("http://www.w3.org/2000/svg", "g"); crosshair.setAttribute("class", "chart-crosshair");
        crosshair.innerHTML = '<line class="crosshair-x"/><line class="crosshair-y"/><circle class="crosshair-point" r="4"/>';
        svg.appendChild(crosshair);
      }
      const v = crosshair.querySelector(".crosshair-x"); v.setAttribute("x1", nearest.px); v.setAttribute("x2", nearest.px); v.setAttribute("y1", current.top); v.setAttribute("y2", current.height - current.bottom);
      const h = crosshair.querySelector(".crosshair-y"); h.setAttribute("x1", current.left); h.setAttribute("x2", current.width - current.right); h.setAttribute("y1", nearest.py); h.setAttribute("y2", nearest.py);
      const dot = crosshair.querySelector(".crosshair-point"); dot.setAttribute("cx", nearest.px); dot.setAttribute("cy", nearest.py);
      state.lastPoint = { x: pxToX, y: nearest.point.y };
      return true;
    };
    svg.addEventListener("pointermove", (event) => { if (!state.pinned) inspect(event); });
    svg.addEventListener("pointerleave", () => {
      if (!state.pinned) { state.readout.textContent = hint(); svg.querySelector(".chart-crosshair")?.remove(); }
    });
    svg.addEventListener("click", (event) => {
      if (state.pinned) {
        state.pinned = false; state.readout.textContent = hint(); svg.querySelector(".chart-crosshair")?.remove();
      } else {
        state.pinned = inspect(event);
      }
    });
    svg.addEventListener("dblclick", (event) => {
      event.preventDefault(); state.pinned = false;
      if (state.baseOptions) makeSvgLineChart(container, state.series, state.baseOptions);
    });
    svg.addEventListener("wheel", (event) => {
      event.preventDefault();
      const pos = coordinate(event); const current = state.layout; const domain = state.domain;
      const fx = Math.min(1, Math.max(0, (pos.x - current.left) / current.plotWidth));
      const fy = Math.min(1, Math.max(0, (pos.y - current.top) / current.plotHeight));
      const cx = domain.xMin + (domain.xMax - domain.xMin) * fx;
      const cy = domain.yMax - (domain.yMax - domain.yMin) * fy;
      const factor = event.deltaY < 0 ? 0.82 : 1 / 0.82;
      const dx = (domain.xMax - domain.xMin) * factor; const dy = (domain.yMax - domain.yMin) * factor;
      const zoom = { xMin: cx - dx * fx, xMax: cx + dx * (1 - fx), yMin: cy - dy * (1 - fy), yMax: cy + dy * fy };
      state.pinned = false;
      makeSvgLineChart(container, state.series, { ...state.options, viewDomain: zoom });
    }, { passive: false });
    state.listenersAttached = true;
  }
  if (!state.pinned) state.readout.textContent = hint();
}

function drawComplexChart(container, tracks, zeros, poles, options) {
  const svg = container.querySelector("svg");
  const width = Math.max(320, Math.round(container.clientWidth || 700));
  const height = Math.max(180, Math.round(container.clientHeight || 260));
  const left = 52; const right = 20; const top = 14; const bottom = 34;
  const all = [...tracks.flatMap((track) => track), ...zeros, ...poles, ...(options && options.lines || []).flatMap((line) => [line.start, line.end])];
  const extent = Math.max(1, ...all.map(absComplex)) * 1.12;
  const side = Math.min(width - left - right, height - top - bottom);
  const plotWidth = Math.max(200, width - left - right); const plotHeight = Math.min(side, height - top - bottom);
  const plotLeft = left + (plotWidth - plotHeight) / 2; const plotTop = top + (height - top - bottom - plotHeight) / 2;
  const sx = (value) => plotLeft + (value + extent) / (2 * extent) * plotHeight;
  const sy = (value) => plotTop + (extent - value) / (2 * extent) * plotHeight;
  const centerX = sx(0); const centerY = sy(0);
  let markup = `<rect class="chart-grid" x="${plotLeft}" y="${plotTop}" width="${plotHeight}" height="${plotHeight}" fill="none"/>`;
  for (let i = -2; i <= 2; i += 1) {
    const value = extent * i / 2;
    markup += `<line class="chart-grid" x1="${sx(value)}" y1="${plotTop}" x2="${sx(value)}" y2="${plotTop+plotHeight}"/><line class="chart-grid" x1="${plotLeft}" y1="${sy(value)}" x2="${plotLeft+plotHeight}" y2="${sy(value)}"/>`;
    if (i !== 0) markup += `<text class="chart-tick" x="${sx(value)}" y="${plotTop+plotHeight+15}" text-anchor="middle">${Number(value.toPrecision(2))}</text><text class="chart-tick" x="${plotLeft-6}" y="${sy(value)+3}" text-anchor="end">${Number(value.toPrecision(2))}</text>`;
  }
  markup += `<line class="axis-zero" x1="${plotLeft}" y1="${centerY}" x2="${plotLeft+plotHeight}" y2="${centerY}"/><line class="axis-zero" x1="${centerX}" y1="${plotTop}" x2="${centerX}" y2="${plotTop+plotHeight}"/>`;
  tracks.forEach((track) => {
    const d = track.map((point, index) => `${index ? "L" : "M"}${sx(point.re).toFixed(2)},${sy(point.im).toFixed(2)}`).join(" ");
    markup += `<path class="root-track" d="${d}"/>`;
  });
  poles.forEach((point) => { markup += `<circle class="root-point" cx="${sx(point.re)}" cy="${sy(point.im)}" r="4"/>`; });
  tracks.forEach((track) => {
    const point = track[track.length - 1];
    if (point) markup += `<circle class="root-end-point" cx="${sx(point.re)}" cy="${sy(point.im)}" r="2.5"/>`;
  });
  zeros.forEach((point) => {
    const x = sx(point.re); const y = sy(point.im);
    markup += `<path class="zero-cross" d="M${x-4},${y-4} L${x+4},${y+4} M${x-4},${y+4} L${x+4},${y-4}"/>`;
  });
  (options && options.lines || []).forEach((line) => {
    const x1 = sx(line.start.re); const y1 = sy(line.start.im); const x2 = sx(line.end.re); const y2 = sy(line.end.im);
    markup += `<line class="root-asymptote" x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}"/>`;
  });
  markup += `<text class="chart-axis-title" x="${plotLeft+plotHeight/2}" y="${height-2}" text-anchor="middle">实轴 Re(s)</text><text class="chart-axis-title" transform="translate(13 ${plotTop+plotHeight/2}) rotate(-90)" text-anchor="middle">虚轴 Im(s)</text>`;
  svg.setAttribute("viewBox", `0 0 ${width} ${height}`);
  svg.innerHTML = markup;
  if (container.__complexChartState) container.__complexChartState.zoom = 1;
  attachComplexChartInteractions(container, tracks, zeros, poles, { width, height, plotLeft, plotTop, plotHeight, extent });
}

function attachComplexChartInteractions(container, tracks, zeros, poles, layout) {
  const svg = container.querySelector("svg");
  let readout = container.nextElementSibling && container.nextElementSibling.classList.contains("chart-readout") ? container.nextElementSibling : null;
  if (!readout) {
    readout = document.createElement("div"); readout.className = "chart-readout"; readout.setAttribute("aria-live", "polite");
    container.insertAdjacentElement("afterend", readout);
  }
  let state = container.__complexChartState;
  if (!state) { state = { zoom: 1, attached: false }; container.__complexChartState = state; }
  state.layout = layout; state.readout = readout;
  if (state.attached) return;
  svg.addEventListener("wheel", (event) => {
    event.preventDefault();
    const current = state.layout;
    state.zoom = Math.min(8, Math.max(0.35, state.zoom * (event.deltaY < 0 ? 1.18 : 1 / 1.18)));
    const viewWidth = current.width / state.zoom; const viewHeight = current.height / state.zoom;
    const screenPoint = svg.createSVGPoint(); screenPoint.x = event.clientX; screenPoint.y = event.clientY;
    const cursor = screenPoint.matrixTransform(svg.getScreenCTM().inverse());
    const currentView = svg.viewBox.baseVal;
    const ratioX = (cursor.x - currentView.x) / currentView.width;
    const ratioY = (cursor.y - currentView.y) / currentView.height;
    const viewX = Math.max(0, Math.min(current.width - viewWidth, cursor.x - viewWidth * ratioX));
    const viewY = Math.max(0, Math.min(current.height - viewHeight, cursor.y - viewHeight * ratioY));
    svg.setAttribute("viewBox", viewX + " " + viewY + " " + viewWidth + " " + viewHeight);
    state.readout.textContent = "根轨迹视图缩放 " + state.zoom.toFixed(2) + "× · 双击恢复";
  }, { passive: false });
  svg.addEventListener("dblclick", () => { state.zoom = 1; const current = state.layout; svg.setAttribute("viewBox", "0 0 " + current.width + " " + current.height); state.readout.textContent = "滚轮缩放 · 双击恢复 · 移动读取 s 平面坐标"; });
  svg.addEventListener("pointermove", (event) => {
    const current = state.layout;
    const screenPoint = svg.createSVGPoint(); screenPoint.x = event.clientX; screenPoint.y = event.clientY;
    const userPoint = screenPoint.matrixTransform(svg.getScreenCTM().inverse());
    const x = userPoint.x; const y = userPoint.y;
    if (x < current.plotLeft || x > current.plotLeft + current.plotHeight || y < current.plotTop || y > current.plotTop + current.plotHeight) return;
    let re = (x - current.plotLeft) / current.plotHeight * 2 * current.extent - current.extent;
    let im = current.extent - (y - current.plotTop) / current.plotHeight * 2 * current.extent;
    const view = svg.viewBox.baseVal; const rect = svg.getBoundingClientRect();
    const scaleX = rect.width / view.width; const scaleY = rect.height / view.height;
    let nearest = null; let nearestPixels = 20;
    [...tracks.flat(), ...zeros, ...poles].forEach((point) => {
      const px = current.plotLeft + (point.re + current.extent) / (2 * current.extent) * current.plotHeight;
      const py = current.plotTop + (current.extent - point.im) / (2 * current.extent) * current.plotHeight;
      const distance = Math.hypot((px - x) * scaleX, (py - y) * scaleY);
      if (distance < nearestPixels) { nearestPixels = distance; nearest = point; }
    });
    if (nearest) { re = nearest.re; im = nearest.im; }
    state.readout.textContent = "s ≈ " + formatComplex({ re, im }, 3) + " · 实轴 Re(s) = " + re.toFixed(3) + " · 虚轴 Im(s) = " + im.toFixed(3);
  });
  state.attached = true;
}

const EXAMPLES = {
  second: { numerator: "25", denominator: "1, 6, 25", duration: 8 },
  first: { numerator: "2", denominator: "1, 2", duration: 5 },
  integrator: { numerator: "1", denominator: "1, 2, 0", duration: 12 },
};

function initApp() {
  const $ = (selector) => document.querySelector(selector);
  const ui = {
    numerator: $("#numerator-input"), denominator: $("#denominator-input"), duration: $("#duration-input"),
    modelInputMode: $("#model-input-mode"), formulaInput: $("#formula-input"), formulaFields: $("#formula-fields"), coefficientFields: $("#coefficient-fields"), responseFields: $("#response-fields"),
    responseTime: $("#response-time-input"), inputSignal: $("#input-signal-input"), outputResponse: $("#output-response-input"),
    example: $("#example-select"), frequencyMode: $("#frequency-mode"), frequencyRange: $("#frequency-range"), gainMax: $("#gain-max"), stabilityGain: $("#stability-gain"),
    asymptoteToggle: $("#asymptote-toggle"), bodeQueryMode: $("#bode-query-mode"), bodeQueryValue: $("#bode-query-value"), bodeQueryPrefix: $("#bode-query-prefix"), bodeQueryUnit: $("#bode-query-unit"), bodeQueryResult: $("#bode-query-result"),
    error: $("#error-message"), state: $("#state-text"), timeOptions: $("#time-options"), frequencyOptions: $("#frequency-options"), rootOptions: $("#root-options"), stabilityOptions: $("#stability-options"),
    timePanel: $("#time-panel"), frequencyPanel: $("#frequency-panel"), rootPanel: $("#root-panel"), stabilityPanel: $("#stability-panel"),
    stepChart: $("#step-chart"), bodeChart: $("#bode-chart"), phaseChart: $("#phase-chart"), nyquistChart: $("#nyquist-chart"), rootChart: $("#root-chart"), stabilityNyquistChart: $("#stability-nyquist-chart"),
    numeratorDisplay: $("#numerator-display"), denominatorDisplay: $("#denominator-display"), modelSummary: $("#model-summary"), metrics: $("#metrics-grid"), bodeAnalysis: $("#bode-analysis"), rootAnalysis: $("#root-analysis"), parameterGrid: $("#parameter-grid"), parameterContext: $("#parameter-context"),
  };
  let currentView = "time";
  let lastModel = null;
  let resizeObserver;

  function setParameterCards(items, context) {
    ui.parameterContext.textContent = context || "分析完成";
    ui.parameterGrid.innerHTML = items.map((item) => '<div class="parameter-item ' + (item.accent ? 'accent' : '') + '"><span class="parameter-item-label">' + item.label + '</span><strong class="parameter-item-value">' + (item.value === null || item.value === undefined || (typeof item.value === "number" && !Number.isFinite(item.value)) ? '—' : item.value) + '<span class="parameter-item-unit">' + (item.unit || '') + '</span></strong></div>').join("");
  }

  function renderModelCards(model) {
    const info = modelCharacteristics(model);
    setParameterCards([
      { label: "增益 K", value: Number(info.gain.toPrecision(5)), unit: "" },
      { label: "系统型别", value: info.type, unit: "型" },
      { label: "相对阶", value: info.relativeOrder, unit: "阶" },
      { label: "开环极点", value: info.poles.length, unit: "个" },
      { label: "开环零点", value: info.zeros.length, unit: "个" },
      { label: "稳定性", value: info.poles.every((pole) => pole.re < -1e-7) ? "稳定" : "待判断", unit: "" },
    ], "模型参数");
  }

  function readSystem() {
    if (ui.modelInputMode.value === "response") {
      const time = parseNumberList(ui.responseTime.value, "时间采样");
      const input = parseNumberList(ui.inputSignal.value, "输入函数");
      const output = parseNumberList(ui.outputResponse.value, "输出响应");
      if (time.length !== input.length || time.length !== output.length || time.length < 2) throw new Error("时间、输入和输出数据必须长度一致，且至少包含两个采样点。");
      if (time.some((value, index) => index && value <= time[index - 1])) throw new Error("时间采样必须严格递增。");
      return { mode: "response", response: { time, input, output } };
    }
    const rawNumerator = ui.modelInputMode.value === "formula" ? parseTransferFunction(ui.formulaInput.value) : { numerator: parsePolynomial(ui.numerator.value), denominator: parsePolynomial(ui.denominator.value) };
    return { mode: "model", model: validateModel(rawNumerator.numerator, rawNumerator.denominator) };
  }

  function updateFormula(system) {
    if (system.mode === "response") {
      ui.numeratorDisplay.textContent = "u(t)";
      ui.denominatorDisplay.textContent = "y(t)";
      ui.modelSummary.textContent = "输入 / 输出采样";
      setParameterCards([{ label: "数据点数", value: system.response.time.length, unit: "点" }, { label: "采样区间", value: Number((system.response.time.at(-1) - system.response.time[0]).toPrecision(4)), unit: "s" }, { label: "输入终值", value: Number(system.response.input.at(-1).toPrecision(5)), unit: "" }, { label: "输出终值", value: Number(system.response.output.at(-1).toPrecision(5)), unit: "" }], "输入 / 输出数据");
      return;
    }
    const model = system.model;
    ui.numeratorDisplay.textContent = formatPolynomial(model.numerator);
    ui.denominatorDisplay.textContent = formatPolynomial(model.denominator);
    const characteristics = modelCharacteristics(model);
    ui.modelSummary.textContent = "K=" + Number(characteristics.gain.toPrecision(4)) + " · 型别 " + characteristics.type + " · 相对阶 " + characteristics.relativeOrder;
    renderModelCards(model);
  }

  function renderMetrics(metrics, overflow, stable) {
    const items = [
      { label: "上升时间", value: metrics.riseTime, unit: "s" },
      { label: "峰值时间", value: metrics.peakTime, unit: "s" },
      { label: "超调量", value: metrics.overshoot, unit: "%" },
      { label: "调节时间", value: metrics.settlingTime, unit: "s" },
    ];
    ui.metrics.innerHTML = items.map((item) => `<div class="metric"><span class="metric-label">${item.label}</span><strong class="metric-value">${item.value === null || item.value === undefined || !Number.isFinite(item.value) ? "—" : item.value.toFixed(item.unit === "%" ? 1 : 2)}<span class="metric-unit">${item.value === null || item.value === undefined || !Number.isFinite(item.value) ? "" : item.unit}</span></strong></div>`).join("");
    const note = $("#time-note");
    if (overflow) note.textContent = "响应在设定仿真范围内数值发散；请缩短时长，或结合根轨迹判断稳定性。";
    else if (!stable) note.textContent = "分母存在非稳定极点；有限时长的仿真值不能代表稳态性能指标。";
    else if (!metrics.valid) note.textContent = "最终值不适用于常规阶跃指标（例如含积分环节）；曲线仍可用于观察响应趋势。";
    else if (metrics.settlingTime === null) note.textContent = "仿真结束前响应尚未进入并保持在最终值的 ±2% 范围内，可增加仿真时长。";
    else note.textContent = `理论稳态值 ${Number.isFinite(metrics.finalValue) ? metrics.finalValue.toFixed(4) : "无穷大 / 不存在"} · 指标采用 2% 调节标准。`;
  }

  function drawTime(model, poles) {
    $("#time-panel .chart-kicker").textContent = "STEP RESPONSE";
    $("#time-panel .chart-heading h2").textContent = "单位阶跃响应";
    const duration = Number(ui.duration.value);
    if (!Number.isFinite(duration) || duration <= 0 || duration > 1000) throw new Error("仿真时长需在 0 到 1000 秒之间。");
    const stable = poles.every((pole) => pole.re < -1e-7);
    const result = simulateStep(model.numerator, model.denominator, duration);
    const finalValue = Math.abs(model.denominator.at(-1)) < COMPLEX_EPSILON ? Infinity : model.numerator.at(-1) / model.denominator.at(-1);
    const metrics = calculateMetrics(result.samples, finalValue);
    if (metrics.valid) {
      const delta = finalValue - result.samples[0].y;
      const direction = Math.sign(delta);
      const peak = result.samples.reduce((best, point) => (point.y - result.samples[0].y) * direction > (best.y - result.samples[0].y) * direction ? point : best, result.samples[0]);
      metrics.peakTime = peak.t;
    }
    const visible = result.samples.filter((point) => Number.isFinite(point.y) && Math.abs(point.y) < 1e12);
    const vals = visible.map((point) => point.y);
    const yMin = Math.min(...vals, Number.isFinite(finalValue) ? finalValue : 0);
    const yMax = Math.max(...vals, Number.isFinite(finalValue) ? finalValue : 0);
    makeSvgLineChart(ui.stepChart, [{ data: visible.map((point) => ({ x: point.t, y: point.y })), fill: true }], {
      xMin: 0, xMax: duration, yMin, yMax, xTicks: 5, yTicks: 4, xLabel: "时间 t (s)", yLabel: "输出 y(t)",
      reference: ui.asymptoteToggle.checked && Number.isFinite(finalValue) && Math.abs(finalValue) < 1e6 ? finalValue : undefined,
    });
    renderMetrics(metrics, result.overflow, stable);
    setParameterCards([
      { label: "上升时间", value: metrics.riseTime === null ? null : metrics.riseTime.toFixed(3), unit: "s" },
      { label: "峰值时间", value: metrics.peakTime === null ? null : metrics.peakTime.toFixed(3), unit: "s" },
      { label: "超调量", value: metrics.overshoot === null ? null : metrics.overshoot.toFixed(2), unit: "%", accent: true },
      { label: "调节时间", value: metrics.settlingTime === null ? null : metrics.settlingTime.toFixed(3), unit: "s" },
      { label: "稳态值", value: Number.isFinite(finalValue) ? finalValue.toFixed(4) : null, unit: "" },
      { label: "极点稳定性", value: stable ? "稳定" : "不稳定", unit: "" },
    ], "时域响应指标");
  }

  function drawResponse(response) {
    $("#time-panel .chart-kicker").textContent = "MEASURED RESPONSE";
    $("#time-panel .chart-heading h2").textContent = "输入 / 输出响应";
    const points = response.time.map((time, index) => ({ x: time, y: response.output[index] }));
    const inputPoints = response.time.map((time, index) => ({ x: time, y: response.input[index] }));
    const finalValue = response.output[response.output.length - 1];
    const metrics = calculateMetrics(points.map((point) => ({ t: point.x, y: point.y })), finalValue);
    const all = response.input.concat(response.output);
    makeSvgLineChart(ui.stepChart, [{ data: inputPoints, className: "input-response-line" }, { data: points, className: "chart-data-line" }], {
      xMin: response.time[0], xMax: response.time.at(-1), yMin: Math.min(...all), yMax: Math.max(...all), xTicks: 5, yTicks: 4, xLabel: "时间 t (s)", yLabel: "幅值",
      reference: ui.asymptoteToggle.checked ? finalValue : undefined,
    });
    const items = [
      { label: "数据点数", value: response.time.length, unit: "点" },
      { label: "输入终值", value: response.input.at(-1), unit: "" },
      { label: "输出终值", value: response.output.at(-1), unit: "" },
      { label: "超调量", value: metrics.valid ? metrics.overshoot : null, unit: "%" },
    ];
    ui.metrics.innerHTML = items.map((item) => '<div class="metric"><span class="metric-label">' + item.label + '</span><strong class="metric-value">' + (item.value === null || !Number.isFinite(item.value) ? "—" : Number(item.value).toFixed(item.unit === "%" ? 1 : 3)) + '<span class="metric-unit">' + item.unit + '</span></strong></div>').join("");
    $("#time-note").textContent = "绿色为输出 y(t)，橙色为输入 u(t)；当前模式用于题目曲线的定性与指标校对。";
    setParameterCards([
      { label: "数据点数", value: response.time.length, unit: "点" },
      { label: "输入终值", value: Number(response.input.at(-1).toPrecision(5)), unit: "" },
      { label: "输出终值", value: Number(response.output.at(-1).toPrecision(5)), unit: "" },
      { label: "超调量", value: metrics.valid ? metrics.overshoot.toFixed(2) : null, unit: "%", accent: true },
      { label: "上升时间", value: metrics.valid && metrics.riseTime !== null ? metrics.riseTime.toFixed(3) : null, unit: "s" },
      { label: "调节时间", value: metrics.valid && metrics.settlingTime !== null ? metrics.settlingTime.toFixed(3) : null, unit: "s" },
    ], "输入 / 输出指标");
  }

  function drawFrequency(model) {
    const decade = Number(ui.frequencyRange.value);
    const minW = Math.pow(10, -decade); const maxW = Math.pow(10, decade);
    const data = Array.from({ length: 320 }, (_, i) => {
      const omega = minW * Math.pow(maxW / minW, i / 319);
      const value = frequencyResponse(model.numerator, model.denominator, omega);
      return { omega, magnitude: 20 * Math.log10(Math.max(absComplex(value), 1e-12)), phase: Math.atan2(value.im, value.re) * 180 / Math.PI, re: value.re, im: value.im };
    });
    let previousPhase = data[0].phase;
    data.forEach((point) => { while (point.phase - previousPhase > 180) point.phase -= 360; while (point.phase - previousPhase < -180) point.phase += 360; previousPhase = point.phase; });
    const nyquist = ui.frequencyMode.value === "nyquist";
    $("#frequency-title").textContent = nyquist ? "Nyquist 图" : "Bode 图";
    $("#frequency-kicker").textContent = nyquist ? "NYQUIST PLOT" : "FREQUENCY RESPONSE";
    ui.bodeChart.hidden = nyquist;
    ui.phaseChart.hidden = nyquist;
    ui.nyquistChart.hidden = !nyquist;
    const roots = modelCharacteristics(model);
    const gainCrossovers = estimateCrossovers(data, "magnitude", 0);
    const phaseMarginPoint = gainCrossovers
      .map((omega) => ({ omega, margin: 180 + interpolateFrequencyField(data, omega, "phase") }))
      .reduce((best, item) => !best || item.margin < best.margin ? item : best, null);
    const gainCrossover = phaseMarginPoint ? phaseMarginPoint.omega : null;
    const phaseCrossover = estimateCrossover(data, "phase", -180);
    const phaseMinForMargins = Math.min(...data.map((point) => point.phase));
    const phaseMaxForMargins = Math.max(...data.map((point) => point.phase));
    const firstOddPhaseIndex = Math.ceil((phaseMinForMargins - 180) / 360);
    const lastOddPhaseIndex = Math.floor((phaseMaxForMargins - 180) / 360);
    const phaseTargets = Array.from({ length: Math.max(0, lastOddPhaseIndex - firstOddPhaseIndex + 1) }, (_, index) => 180 + 360 * (firstOddPhaseIndex + index));
    const gainMarginPoint = phaseTargets.flatMap((target) => estimateCrossovers(data, "phase", target).map((omega) => ({
      omega, phase: target, margin: -interpolateFrequencyField(data, omega, "magnitude"),
    }))).reduce((best, item) => !best || item.margin < best.margin ? item : best, null);
    const phaseMargin = phaseMarginPoint ? phaseMarginPoint.margin : null;
    const gainMargin = gainMarginPoint ? gainMarginPoint.margin : Infinity;
    setParameterCards([
      { label: "增益 K", value: Number(roots.gain.toPrecision(5)), unit: "" },
      { label: "型别", value: roots.type, unit: "型" },
      { label: "相对阶", value: roots.relativeOrder, unit: "阶" },
      { label: "相位裕度", value: phaseMargin === null ? null : phaseMargin.toFixed(2), unit: "°", accent: true },
      { label: "幅值裕度", value: gainMargin === Infinity ? "∞" : gainMargin.toFixed(2), unit: "dB", accent: true },
      { label: "0 dB 频率", value: gainCrossover === null ? null : gainCrossover.toFixed(3), unit: "rad/s" },
    ], "频域特性指标");
    if (nyquist) {
      const all = data.flatMap((point) => [point.re, point.im]);
      const range = Math.max(1, ...all.map(Math.abs));
      makeSvgLineChart(ui.nyquistChart, [{ data: data.map((p) => ({ x: p.re, y: p.im })), className: "nyquist-line" }], {
        xMin: -range * 1.1, xMax: range * 1.1, yMin: -range * 1.1, yMax: range * 1.1, xTicks: 4, yTicks: 4,
        xLabel: "实部 Re[G(jω)]", yLabel: "虚部 Im[G(jω)]", formatX: (v) => Number(v.toPrecision(2)).toString(), formatY: (v) => Number(v.toPrecision(2)).toString(), padding: false,
      });
      $("#frequency-note").textContent = `绘制正频率支路，ω 从 ${minW} 到 ${maxW} rad/s；负频率支路关于实轴对称。`;
      ui.bodeAnalysis.innerHTML = "<div class=\"analysis-row\"><span>增益</span><strong>" + Number(roots.gain.toPrecision(5)) + "</strong><span>型别</span><strong>" + roots.type + "</strong><span>相对阶</span><strong>" + roots.relativeOrder + "</strong><span>相位裕度</span><strong>" + (phaseMargin === null ? '—' : phaseMargin.toFixed(2) + '°') + "</strong><span>幅值裕度</span><strong>" + (gainMargin === Infinity ? '∞ dB' : gainMargin.toFixed(2) + ' dB') + "</strong></div>";
    } else {
      const xValues = data.map((point) => Math.log10(point.omega));
      const minX = xValues[0]; const maxX = xValues[xValues.length - 1];
      const superscriptDigits = { "-": "⁻", "0": "⁰", "1": "¹", "2": "²", "3": "³", "4": "⁴", "5": "⁵", "6": "⁶", "7": "⁷", "8": "⁸", "9": "⁹" };
      const formatter = (value) => "10" + String(Math.round(value)).split("").map((digit) => superscriptDigits[digit] || digit).join("");
      const decimated = data.filter((_, i) => i % 2 === 0);
      const magnitudeMin = Math.min(...data.map((point) => point.magnitude));
      const magnitudeMax = Math.max(...data.map((point) => point.magnitude));
      const cornerFrequencies = dedupeNumbers(roots.corners.map((item) => item.omega)).filter((omega) => omega >= minW && omega <= maxW);
      const verticals = ui.asymptoteToggle.checked ? cornerFrequencies.map((omega) => ({ x: Math.log10(omega), label: "ω=" + Number(omega.toPrecision(3)) })) : [];
      const asymptotic = data.map((point) => ({ x: Math.log10(point.omega), y: asymptoticMagnitudeDb(model, roots, point.omega) }));
      const crossover = estimateCrossover(data, "magnitude", 0);
      makeSvgLineChart(ui.bodeChart, [{ name: "幅频曲线", data: decimated.map((p) => ({ x: Math.log10(p.omega), y: p.magnitude, omega: p.omega })) }, ...(ui.asymptoteToggle.checked ? [{ name: "渐近线", data: asymptotic, className: "asymptote-line" }] : [])], {
        xMin: minX, xMax: maxX, xTicks: decade * 2, yTicks: 4, xLabel: "角频率 ω (rad/s)", yLabel: "幅值 |G| (dB)",
        yMin: magnitudeMin, yMax: magnitudeMax, formatX: formatter, verticals, points: crossover ? [{ x: Math.log10(crossover), y: 0, label: "ωc=" + Number(crossover.toPrecision(3)) }] : [], formatReadoutX: (value) => "ω=" + Math.pow(10, value).toPrecision(5) + " rad/s · f=" + (Math.pow(10, value) / (2 * Math.PI)).toPrecision(5) + " Hz", formatReadoutY: (value) => value.toFixed(3) + " dB",
      });
      const phaseMin = Math.min(...data.map((point) => point.phase)); const phaseMax = Math.max(...data.map((point) => point.phase));
      const phaseCross = phaseCrossover;
      makeSvgLineChart(ui.phaseChart, [{ name: "相频曲线", data: decimated.map((p) => ({ x: Math.log10(p.omega), y: p.phase, omega: p.omega })) }], {
        xMin: minX, xMax: maxX, xTicks: decade * 2, yTicks: 4, xLabel: "角频率 ω (rad/s)", yLabel: "相角 ∠G (°)",
        yMin: phaseMin, yMax: phaseMax, formatX: formatter, formatY: (v) => `${Math.round(v)}°`, verticals, points: phaseCross ? [{ x: Math.log10(phaseCross), y: -180, label: "ωπ=" + Number(phaseCross.toPrecision(3)) }] : [], formatReadoutX: (value) => "ω=" + Math.pow(10, value).toPrecision(5) + " rad/s · f=" + (Math.pow(10, value) / (2 * Math.PI)).toPrecision(5) + " Hz", formatReadoutY: (value) => value.toFixed(2) + "°",
      });
      const slopeBreaks = [minW, ...cornerFrequencies.filter((omega) => omega > minW && omega < maxW), maxW];
      const slopes = slopeBreaks.slice(0, -1).map((start, index) => {
        const end = slopeBreaks[index + 1];
        const sample = Math.sqrt(start * end);
        return Number(start.toPrecision(3)) + "–" + Number(end.toPrecision(3)) + " rad/s：" + asymptoticSlopeDbDecade(roots, sample) + " dB/dec";
      });
      ui.bodeAnalysis.innerHTML = '<div class="analysis-row"><span>增益</span><strong>' + Number(roots.gain.toPrecision(5)) + '</strong><span>型别</span><strong>' + roots.type + '</strong><span>相对阶</span><strong>' + roots.relativeOrder + '</strong><span>相位裕度</span><strong>' + (phaseMargin === null ? '—' : phaseMargin.toFixed(2) + '°') + '</strong><span>幅值裕度</span><strong>' + (gainMargin === Infinity ? '∞ dB' : gainMargin.toFixed(2) + ' dB') + '</strong></div><div class="analysis-grid"><span>转折频率</span><b>' + (roots.corners.length ? roots.corners.map((item) => item.kind + ' ' + Number(item.omega.toPrecision(4))).join(' · ') : '无') + '</b><span>0 dB 截止频率</span><b>' + (gainCrossover ? gainCrossover.toPrecision(5) + ' rad/s' : '当前范围内无') + '</b><span>相位裕度频率</span><b>' + (phaseMarginPoint ? phaseMarginPoint.omega.toPrecision(5) + ' rad/s' : '当前范围内无') + '</b><span>−180° 频率</span><b>' + (phaseCross ? phaseCross.toPrecision(5) + ' rad/s' : '当前范围内无') + '</b><span>幅值裕度频率</span><b>' + (gainMarginPoint ? gainMarginPoint.omega.toPrecision(5) + ' rad/s（相位 ' + gainMarginPoint.phase + '°）' : '当前范围内无') + '</b><span>斜率</span><b>' + (slopes.length ? slopes.join('；') : asymptoticSlopeDbDecade(roots, minW) + ' dB/dec') + '</b></div>';
      $("#frequency-note").textContent = `频率范围 ${minW} – ${maxW} rad/s · 相位按频率连续展开。`;
      ui.bodeQueryResult.textContent = evaluateQuery(data, ui.bodeQueryMode.value, Number(ui.bodeQueryValue.value));
    }
  }

  function drawRoot(model) {
    const maxGain = Number(ui.gainMax.value);
    if (!Number.isFinite(maxGain) || maxGain <= 0 || maxGain > 10000) throw new Error("增益上限需在 0 到 10000 之间。");
    const den = model.denominator; const num = model.numerator;
    const n = den.length; const paddedNum = new Array(n).fill(0);
    for (let i = 0; i < num.length; i += 1) paddedNum[n - num.length + i] = num[i];
    const gains = [0, ...Array.from({ length: 47 }, (_, i) => maxGain * Math.pow(0.001, 1 - (i + 1) / 48))];
    gains[gains.length - 1] = maxGain;
    const tracks = Array.from({ length: n - 1 }, () => []);
    let previous = null;
    for (const gain of gains) {
      const coeffs = den.map((coefficient, i) => coefficient + gain * paddedNum[i]);
      const roots = polynomialRoots(coeffs);
      if (!previous) {
        roots.forEach((root, i) => { tracks[i].push(root); });
      } else {
        const remaining = new Set(roots.map((_, index) => index));
        previous.forEach((root, i) => {
          let nearest = null; let nearestDistance = Infinity;
          for (const candidateIndex of remaining) {
            const candidate = roots[candidateIndex];
            const distance = Math.hypot(root.re - candidate.re, root.im - candidate.im);
            if (distance < nearestDistance) { nearestDistance = distance; nearest = candidateIndex; }
          }
          if (nearest !== null) { tracks[i].push(roots[nearest]); remaining.delete(nearest); }
        });
      }
      previous = roots;
    }
    const zeros = polynomialRoots(num);
    const poles = polynomialRoots(den);
    const rootInfo = modelCharacteristics(model);
    const asymptoteCount = Math.max(0, poles.length - zeros.length);
    const centroid = asymptoteCount ? poles.reduce((sum, point) => sum + point.re, 0) / asymptoteCount - zeros.reduce((sum, point) => sum + point.re, 0) / asymptoteCount : null;
    const angles = asymptoteCount ? Array.from({ length: asymptoteCount }, (_, index) => (2 * index + 1) * 180 / asymptoteCount) : [];
    const asymptoteLines = centroid === null ? [] : angles.map((angle) => {
      const radians = angle * Math.PI / 180; const reach = Math.max(10, maxGain * 0.15, ...poles.map(absComplex), ...zeros.map(absComplex));
      return { start: { re: centroid, im: 0 }, end: { re: centroid + reach * Math.cos(radians), im: reach * Math.sin(radians) } };
    });
    drawComplexChart(ui.rootChart, tracks, zeros, poles, { lines: ui.asymptoteToggle.checked ? asymptoteLines : [] });
    const breakaway = findBreakawayPoints(model, rootInfo);
    const imaginary = findImaginaryCrossings(model);
    const equation = formatPolynomial(model.denominator) + " + K(" + formatPolynomial(model.numerator) + ") = 0";
    ui.rootAnalysis.innerHTML = '<div class="analysis-grid"><span>传递函数</span><b>G(s) = (' + formatPolynomial(model.numerator) + ')/(' + formatPolynomial(model.denominator) + ')</b><span>特征方程</span><b>' + equation + '</b><span>零点</span><b>' + (zeros.length ? zeros.map((point) => formatComplex(point)).join('，') : '无有限零点') + '</b><span>极点</span><b>' + (poles.length ? poles.map((point) => formatComplex(point)).join('，') : '无有限极点') + '</b><span>阶数差</span><b>' + asymptoteCount + '</b><span>渐近线交点</span><b>' + (centroid === null ? '无' : 's = ' + centroid.toFixed(4)) + '</b><span>渐近线角度</span><b>' + (angles.length ? angles.map((angle) => angle.toFixed(1) + '°').join('，') : '无') + '</b><span>分离 / 汇合点</span><b>' + (breakaway.length ? breakaway.map((point) => 's = ' + point.toFixed(4)).join('，') : '当前范围内未找到') + '</b><span>穿越 jω</span><b>' + (imaginary.length ? imaginary.map((item) => 'K=' + item.gain.toPrecision(4) + '，ω=' + item.omega.toPrecision(4)).join('；') : '当前范围内未找到') + '</b></div>';
    setParameterCards([
      { label: "阶数差", value: asymptoteCount, unit: "阶" },
      { label: "渐近线交点", value: centroid === null ? null : centroid.toFixed(3), unit: "" },
      { label: "渐近线数量", value: angles.length, unit: "条" },
      { label: "分离 / 汇合点", value: breakaway.length, unit: "个", accent: true },
      { label: "jω 穿越点", value: imaginary.length, unit: "个", accent: true },
      { label: "K 上限", value: maxGain, unit: "" },
    ], "根轨迹关键量");
    const unstableAtEnd = previous?.some((pole) => pole.re > 0) ?? false;
    $("#root-note").textContent = unstableAtEnd ? `K = ${maxGain} 时至少一个闭环极点位于右半平面；请结合完整根轨迹观察稳定性变化。` : `K 从 0 增至 ${maxGain}；实轴右侧代表闭环不稳定极点所在区域。`;
  }

  function drawStability(model) {
    const gain = Number(ui.stabilityGain.value);
    if (!Number.isFinite(gain) || gain <= 0 || gain > 10000) throw new Error("反馈增益 K 需在 0 到 10000 之间。");
    const polynomial = closedLoopPolynomial(model, gain);
    const table = routhTable(polynomial);
    const roots = polynomialRoots(polynomial);
    const imaginaryClosedPole = roots.some((root) => Math.abs(root.re) < 1e-7);
    const stable = table.signChanges === 0 && !imaginaryClosedPole;
    const status = table.signChanges > 0 ? "不稳定" : (imaginaryClosedPole ? "临界稳定" : "稳定");
    const statusClass = stable ? "" : " fail";
    $("#stability-gain-label").textContent = Number(gain.toPrecision(5));
    $("#stability-summary").innerHTML = '<div class="stability-status' + statusClass + '"><span class="status-dot-large"></span>劳斯判据：' + status + ' <span class="text-muted">（第一列变号 ' + table.signChanges + ' 次）</span></div>';
    const columns = Math.ceil((table.order + 1) / 2);
    let html = '<table><thead><tr><th>s 次方</th>';
    for (let column = 0; column < columns; column += 1) html += '<th>列 ' + (column + 1) + '</th>';
    html += '</tr></thead><tbody>';
    table.table.forEach((row, index) => {
      html += '<tr><td>s<sup>' + (table.order - index) + '</sup></td>';
      row.forEach((value) => { const className = value < -1e-8 ? 'negative' : Math.abs(value) < 1e-8 ? 'zero' : ''; html += '<td class="' + className + '">' + (Number.isFinite(value) ? Number(value.toPrecision(5)) : '—') + '</td>'; });
      html += '</tr>';
    });
    html += '</tbody></table>';
    $("#routh-table").innerHTML = html;
    const data = nyquistData(model, 1, 500);
    const critical = -1 / gain;
    const all = [...data.negative, ...(data.dc ? [data.dc] : []), ...data.positive];
    const range = Math.max(1.5, ...all.flatMap((point) => [Math.abs(point.x), Math.abs(point.y), Math.abs(critical)])) * 1.12;
    makeSvgLineChart(ui.stabilityNyquistChart, [{ name: "负频率支路", data: data.negative.map((point) => ({ x: point.x, y: point.y })) }, { name: "正频率支路", data: data.positive.map((point) => ({ x: point.x, y: point.y })) }, ...(data.dc ? [{ name: "直流点", data: [{ x: data.dc.x, y: data.dc.y }] }] : [])], {
      xMin: -range, xMax: range, yMin: -range, yMax: range, xTicks: 4, yTicks: 4, xLabel: "Re[G(jω)]", yLabel: "Im[G(jω)]", padding: false,
      points: [{ x: critical, y: 0, label: "−1/K" }], formatReadoutX: (value) => Number(value.toPrecision(5)).toString(), formatReadoutY: (value) => Number(value.toPrecision(5)).toString(),
    });
    const winding = nyquistWinding(data, critical);
    const openLoopPoles = modelCharacteristics(model).poles;
    const pCount = openLoopPoles.filter((pole) => pole.re > 1e-7).length;
    const zCount = data.imaginaryAxisPole || imaginaryClosedPole ? null : pCount - winding;
    const nyquistStable = zCount === null ? null : zCount === 0;
    const nyquistStatus = data.imaginaryAxisPole ? "需绕行" : (imaginaryClosedPole ? "临界稳定" : (nyquistStable ? "稳定" : "不稳定"));
    $("#nyquist-summary").innerHTML = '<span>临界点</span><b>(' + critical.toPrecision(5) + ', 0)</b><span>开环右半平面极点 P</span><b>' + pCount + '</b><span>有向环绕数 N</span><b>' + (data.imaginaryAxisPole ? '需绕开虚轴极点' : (imaginaryClosedPole ? '经过临界点' : winding)) + '</b><span>闭环右半平面零点 Z</span><b>' + (zCount === null ? (imaginaryClosedPole ? '临界点位于曲线上' : '需作绕行计算') : zCount) + '</b><span>Nyquist 判据</span><b>' + nyquistStatus + '</b><span>闭环极点</span><b>' + roots.map((root) => formatComplex(root)).join('，') + '</b>';
    $("#stability-note").textContent = data.imaginaryAxisPole ? "开环含虚轴极点，严格 Nyquist 判据需对该极点作小半圆绕行；本工具在此情形下以劳斯表和闭环极点给出结论，环绕数仅供观察。" : (imaginaryClosedPole ? "闭环极点位于虚轴，Nyquist 曲线经过临界点 −1/K，属于临界稳定，不能按普通环绕数直接计数。" : "Nyquist 曲线按正、负频率闭合；此处绘制 G(jω)，并以 −1/K 为单位负反馈临界点。");
    setParameterCards([
      { label: "劳斯判据", value: status, unit: "", accent: !stable },
      { label: "第一列变号", value: table.signChanges, unit: "次" },
      { label: "Nyquist 判据", value: nyquistStatus, unit: "", accent: nyquistStable === false || imaginaryClosedPole },
      { label: "Nyquist 环绕数", value: data.imaginaryAxisPole || imaginaryClosedPole ? null : winding, unit: "次" },
      { label: "反馈增益 K", value: Number(gain.toPrecision(5)), unit: "" },
      { label: "闭环极点", value: roots.length, unit: "个" },
    ], "稳定性判据");
  }

  function calculate() {
    ui.error.hidden = true;
    try {
      const system = readSystem();
      lastModel = system;
      updateFormula(system);
      if (currentView === "time") {
        if (system.mode === "response") drawResponse(system.response);
        else drawTime(system.model, polynomialRoots(system.model.denominator));
      }
      if (currentView === "frequency") {
        if (system.mode === "response") throw new Error("输入 / 输出采样模式只支持时域曲线和指标校对，请切回时域响应或输入传递函数。");
        drawFrequency(system.model);
      }
      if (currentView === "root") {
        if (system.mode === "response") throw new Error("根轨迹需要传递函数模型，请切回公式或系数输入。");
        drawRoot(system.model);
      }
      if (currentView === "stability") {
        if (system.mode === "response") throw new Error("稳定性判据需要传递函数模型，请切回公式或系数输入。");
        drawStability(system.model);
      }
      ui.state.textContent = "分析完成";
    } catch (error) {
      ui.error.textContent = error instanceof Error ? error.message : "参数有误，请检查后重试。";
      ui.error.hidden = false;
      ui.state.textContent = "参数待检查";
    }
  }

  function setView(view) {
    currentView = view;
    const views = {
      time: { title: "时域响应", description: "用单位阶跃响应检验系统的动态性能指标。", panel: ui.timePanel, options: ui.timeOptions },
      frequency: { title: "频域特性", description: "观察系统的频率响应，校验幅值与相位变化。", panel: ui.frequencyPanel, options: ui.frequencyOptions },
      root: { title: "根轨迹", description: "观察开环增益变化时，闭环极点在 s 平面中的运动。", panel: ui.rootPanel, options: ui.rootOptions },
      stability: { title: "稳定性判据", description: "用劳斯表与 Nyquist 曲线交叉校验闭环稳定性。", panel: ui.stabilityPanel, options: ui.stabilityOptions },
    };
    const selected = views[view];
    if (ui.modelInputMode.value === "response" && view !== "time") {
      ui.error.textContent = "输入 / 输出采样模式只支持时域响应。切换到传递函数公式后可使用频域和根轨迹。";
      ui.error.hidden = false;
    }
    $("#page-title").innerHTML = `${selected.title}<span class="title-period">.</span>`;
    $("#page-description").textContent = selected.description;
    $("#breadcrumb-current").textContent = selected.title;
    Object.values(views).forEach((entry) => { entry.panel.hidden = entry !== selected; entry.options.hidden = entry !== selected; });
    document.querySelectorAll("[data-view]").forEach((button) => {
      const active = button.dataset.view === view;
      button.classList.toggle("active", active);
      if (button.matches("[role=tab]")) button.setAttribute("aria-selected", String(active));
      if (button.classList.contains("nav-item")) active ? button.setAttribute("aria-current", "page") : button.removeAttribute("aria-current");
    });
    calculate();
  }

  document.querySelectorAll("[data-view]").forEach((button) => button.addEventListener("click", () => setView(button.dataset.view)));
  $("#calculate-button").addEventListener("click", calculate);
  ui.modelInputMode.addEventListener("change", () => {
    const mode = ui.modelInputMode.value;
    ui.formulaFields.hidden = mode !== "formula";
    ui.coefficientFields.hidden = mode !== "coefficients";
    ui.responseFields.hidden = mode !== "response";
    calculate();
  });
  ui.example.addEventListener("change", () => {
    const example = EXAMPLES[ui.example.value];
    if (!example) return;
    ui.modelInputMode.value = "coefficients";
    ui.formulaFields.hidden = true; ui.coefficientFields.hidden = false; ui.responseFields.hidden = true;
    ui.numerator.value = example.numerator;
    ui.denominator.value = example.denominator;
    ui.duration.value = example.duration;
    calculate();
  });
  [ui.frequencyMode, ui.frequencyRange, ui.gainMax, ui.stabilityGain, ui.asymptoteToggle].forEach((input) => input.addEventListener("change", calculate));
  [ui.numerator, ui.denominator, ui.formulaInput, ui.responseTime, ui.inputSignal, ui.outputResponse, ui.duration].forEach((input) => input.addEventListener("keydown", (event) => { if (event.key === "Enter" && !event.shiftKey) calculate(); }));
  ui.bodeQueryMode.addEventListener("change", () => {
    const phaseMode = ui.bodeQueryMode.value === "phase-at-frequency";
    ui.bodeQueryPrefix.textContent = phaseMode ? "ω" : "φ";
    ui.bodeQueryUnit.textContent = phaseMode ? "rad/s" : "deg";
    if (lastModel && lastModel.mode === "model" && currentView === "frequency") calculate();
  });
  ui.bodeQueryValue.addEventListener("input", () => { if (lastModel && lastModel.mode === "model" && currentView === "frequency") calculate(); });
  if (typeof ResizeObserver !== "undefined") {
    resizeObserver = new ResizeObserver(() => { if (lastModel) calculate(); });
    [ui.stepChart, ui.bodeChart, ui.phaseChart, ui.nyquistChart, ui.rootChart, ui.stabilityNyquistChart].forEach((chart) => resizeObserver.observe(chart));
  } else window.addEventListener("resize", () => { if (lastModel) calculate(); });
  calculate();
}

const ControlLabMath = { parsePolynomial, parseTransferFunction, parseNumberList, polynomialRoots, validateModel, simulateStep, calculateMetrics, frequencyResponse, formatPolynomial, modelCharacteristics, findBreakawayPoints, findImaginaryCrossings, estimateCrossover, estimateCrossovers, interpolateFrequencyField, routhTable, closedLoopPolynomial, nyquistData, nyquistWinding };
if (typeof module !== "undefined" && module.exports) module.exports = ControlLabMath;
if (typeof document !== "undefined") initApp();
