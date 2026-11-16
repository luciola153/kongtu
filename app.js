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
  const scale = Math.max(Math.abs(b.re), Math.abs(b.im));
  if (scale === 0) return { re: NaN, im: NaN };
  const re = b.re / scale; const im = b.im / scale; const d = re * re + im * im;
  return { re: ((a.re / scale) * re + (a.im / scale) * im) / d, im: ((a.im / scale) * re - (a.re / scale) * im) / d };
}
function absComplex(a) { return Math.hypot(a.re, a.im); }
function evalPolynomial(coefficients, z) {
  let value = { re: coefficients[0], im: 0 };
  for (let i = 1; i < coefficients.length; i += 1) value = addComplex(mulComplex(value, z), { re: coefficients[i], im: 0 });
  return value;
}

function polynomialRoots(coefficients) {
  coefficients = trimPolynomial(coefficients);
  const originRoots = [];
  while (coefficients.length > 1 && coefficients.at(-1) === 0) {
    originRoots.push({ re: 0, im: 0 });
    coefficients = coefficients.slice(0, -1);
  }
  const degree = coefficients.length - 1;
  if (degree <= 0) return originRoots;
  const normalized = coefficients.map((value) => value / coefficients[0]);
  if (degree === 1) return originRoots.concat([{ re: -normalized[1], im: 0 }]);
  const rootScale = Math.max(1e-12, ...normalized.slice(1).map((value, i) => Math.pow(Math.abs(value), 1 / (i + 1))));
  const scaled = normalized.map((value, i) => value / Math.pow(rootScale, i));
  const radius = 1 + Math.max(...scaled.slice(1).map(Math.abs));
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
      const shift = divComplex(evalPolynomial(scaled, root), divisor);
      if (!Number.isFinite(shift.re) || !Number.isFinite(shift.im)) return root;
      maxShift = Math.max(maxShift, absComplex(shift));
      return subComplex(root, shift);
    });
    roots = next;
    if (maxShift < 1e-12) break;
  }
  return originRoots.concat(roots.map((root) => {
    const re = root.re * rootScale; const im = root.im * rootScale;
    return { re: Math.abs(re) < 1e-10 ? 0 : re, im: Math.abs(im) < 1e-10 ? 0 : im };
  }));
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
    if (!Number.isFinite(phase) || !Number.isFinite(magnitude)) return "该频率位于奇点或奇点邻域，频率响应未定义，不能跨越奇点插值。";
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
    const numerator = evalPolynomial(model.numerator, { re: s, im: 0 }).re;
    const denominator = evalPolynomial(model.denominator, { re: s, im: 0 }).re;
    const gain = Math.abs(numerator) < 1e-10 ? -Infinity : -denominator / numerator;
    const realPointsRight = [...roots.poles, ...roots.zeros].filter((point) => Math.abs(point.im) < 1e-6 && point.re > s).length;
    return gain > 0 && (realPointsRight % 2 === 1) === (model.numerator[0] / model.denominator[0] > 0);
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
  const epsilonRows = [];
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
    if (Math.abs(table[row - 1][0]) < epsilon) { table[row - 1][0] = epsilon; epsilonRows.push(row - 1); }
    for (let column = 0; column < columns - 1; column += 1) {
      const numerator = table[row - 1][0] * table[row - 2][column + 1] - table[row - 2][0] * table[row - 1][column + 1];
      table[row][column] = numerator / table[row - 1][0];
    }
  }
  const firstColumn = table.map((row) => row[0]);
  const signs = firstColumn.map((value) => Math.sign(value));
  let signChanges = 0;
  for (let i = 1; i < signs.length; i += 1) if (signs[i] && signs[i - 1] && signs[i] !== signs[i - 1]) signChanges += 1;
  return { table, order, firstColumn, signChanges, specialRows, epsilonRows };
}

function closedLoopPolynomial(model, gain) {
  const length = Math.max(model.denominator.length, model.numerator.length);
  const denominator = new Array(length - model.denominator.length).fill(0).concat(model.denominator);
  const numerator = new Array(length - model.numerator.length).fill(0).concat(model.numerator);
  return addPolynomial(denominator, scalePolynomial(numerator, gain));
}

const FAR_RADIUS = 10000;
const finitePoint = (p) => p && Number.isFinite(p.x) && Number.isFinite(p.y);
function niceStep(span, target = 6) {
  const raw = Math.max(span / target, 1e-12), base = Math.pow(10, Math.floor(Math.log10(raw)));
  return ([1, 2, 2.5, 5, 10].find((factor) => factor >= raw / base) || 10) * base;
}
function axisTicks(min, max, step = niceStep(max - min), aligned = false) {
  if (!(Number.isFinite(min) && Number.isFinite(max) && Number.isFinite(step) && max > min && step > 0)) throw new Error("刻度需要：最大值 > 最小值，每格步长 > 0。");
  if ((max - min) / step > 200) throw new Error("每个坐标轴最多 200 格，请增大刻度步长。");
  const start = aligned ? Math.ceil(min / step - 1e-10) * step : min;
  return Array.from({ length: Math.floor((max - start) / step + 1e-8) + 1 }, (_, i) => start + i * step);
}
function niceBounds(min, max, reference) {
  if (!Number.isFinite(min) || !Number.isFinite(max)) return { min: -1, max: 1, step: 0.5 };
  if (reference !== undefined) { min = Math.min(min, reference); max = Math.max(max, reference); }
  const pad = Math.max(max - min, 1) * 0.08, step = niceStep(max - min + 2 * pad);
  return { min: Math.floor((min - pad) / step) * step, max: Math.ceil((max + pad) / step) * step, step };
}
const characteristicsCache = new WeakMap();
function modelCharacteristicsCached(model) {
  if (!characteristicsCache.has(model)) characteristicsCache.set(model, modelCharacteristics(model));
  return characteristicsCache.get(model);
}
function frequencySamples(model, minW, maxW, count = 1200) {
  const roots = modelCharacteristicsCached(model);
  const poles = roots.poles.filter((p) => Math.abs(p.re) < 1e-7 && p.im > 1e-7).map((p) => p.im);
  const values = Array.from({ length: count }, (_, i) => minW * Math.pow(maxW / minW, i / (count - 1)));
  poles.forEach((omega) => {
    if (omega >= minW && omega <= maxW) {
      values.push(omega);
      for (const delta of [1e-3, 1e-5, 1e-7]) values.push(omega * (1 - delta), omega * (1 + delta));
    }
  });
  let previousPhase = null;
  return [...new Set(values)].sort((a, b) => a - b).filter((w) => w >= minW && w <= maxW).map((omega) => {
    if (poles.some((p) => Math.abs(omega - p) < 1e-10 * Math.max(1, p))) {
      previousPhase = null; return { omega, magnitude: NaN, phase: NaN, re: NaN, im: NaN };
    }
    const response = frequencyResponse(model.numerator, model.denominator, omega);
    let phase = Math.atan2(response.im, response.re) * 180 / Math.PI;
    const factorPhase = (list) => list.reduce((sum, p) => sum + Math.atan2(omega - p.im, -p.re) * 180 / Math.PI, 0);
    const target = (model.numerator[0] < 0 ? 180 : 0) + factorPhase(roots.zeros) - factorPhase(roots.poles);
    phase += Math.round((target - phase) / 360) * 360;
    if (previousPhase !== null && !poles.some((p) => p > previousPhase.omega && p < omega)) {
      while (phase - previousPhase.phase > 180) phase -= 360;
      while (phase - previousPhase.phase < -180) phase += 360;
    }
    previousPhase = { omega, phase };
    return { omega, magnitude: 20 * Math.log10(absComplex(response)), phase, re: response.re, im: response.im };
  });
}
function nyquistData(model, gain = 1, samples = 800) {
  const roots = modelCharacteristicsCached(model);
  const axisPoles = roots.poles.filter((p) => Math.abs(p.re) < 1e-7 * Math.max(1, absComplex(p)));
  const positivePoles = dedupeNumbers(axisPoles.map((p) => p.im).filter((w) => w > 1e-7));
  const originPole = axisPoles.some((p) => Math.abs(p.im) < 1e-7), frequencies = [0, ...positivePoles];
  const maxFrequency = Math.max(1, ...roots.poles.map(absComplex), ...roots.zeros.map(absComplex)) * 1e6;
  const mapAt = (s) => {
    const response = divComplex(evalPolynomial(model.numerator, s), evalPolynomial(model.denominator, s));
    return { x: gain * response.re, y: gain * response.im };
  };
  const epsAt = (w) => Math.max(1e-12, Math.min(1e-7 * Math.max(1, w),
    ...frequencies.filter((q) => q !== w).map((q) => Math.abs(q - w) * 1e-5)));
  const positiveBranches = [];
  for (let i = 0; i < frequencies.length; i += 1) {
    const low = frequencies[i], high = frequencies[i + 1], lowPole = i > 0 || originPole;
    const lo = low + (lowPole ? epsAt(low) : Math.max(1e-10, maxFrequency * 1e-14)), values = [];
    if (high !== undefined) {
      const hi = high - epsAt(high), span = high - low;
      const logit = (w) => Math.log((w - low) / (high - w)), t0 = logit(lo), t1 = logit(hi);
      for (let j = 0; j <= samples; j += 1) values.push(low + span / (1 + Math.exp(-(t0 + (t1 - t0) * j / samples))));
    } else {
      const start = lo - low;
      for (let j = 0; j <= samples; j += 1) values.push(low + start * Math.pow(maxFrequency / start, j / samples));
    }
    if (i === 0 && !originPole) values.unshift(0);
    positiveBranches.push({ name: "正频率支路 " + (i + 1), data: values.map((omega) => ({ omega, ...mapAt({ re: 0, im: omega }) })), low, high: high ?? Infinity });
  }
  const detours = frequencies.filter((w) => w > 0 || originPole).map((omega) => {
    const epsilon = epsAt(omega), startAngle = omega === 0 ? 0 : -Math.PI / 2;
    const data = Array.from({ length: 361 }, (_, i) => {
      const theta = startAngle + (Math.PI / 2 - startAngle) * i / 360;
      return { ...mapAt({ re: epsilon * Math.cos(theta), im: omega + epsilon * Math.sin(theta) }), contour: true };
    });
    return { name: "绕行映射 ω=" + Number(omega.toPrecision(5)), omega, epsilon, data };
  });
  const positiveContour = [];
  if (originPole) positiveContour.push(...detours.find((d) => d.omega === 0).data);
  positiveBranches.forEach((branch) => {
    positiveContour.push(...branch.data);
    if (Number.isFinite(branch.high)) positiveContour.push(...detours.find((d) => d.omega === branch.high).data);
  });
  const negativeBranches = positiveBranches.slice().reverse().map((branch) => ({
    name: branch.name.replace("正", "负"), data: branch.data.slice().reverse().map((p) => ({ ...p, omega: -p.omega, y: -p.y })),
  }));
  const contour = positiveContour.slice().reverse().map((p) => ({ x: p.x, y: -p.y })).concat(positiveContour);
  const derivativeDen = polynomialDerivative(model.denominator), secondDen = polynomialDerivative(derivativeDen), derivativeNum = polynomialDerivative(model.numerator);
  const asymptotes = [], limits = [];
  frequencies.filter((w) => w > 0 || originPole).forEach((omega) => {
    const p = { re: 0, im: omega }, d1 = evalPolynomial(derivativeDen, p);
    if (axisPoles.filter((q) => Math.abs(q.im - omega) < 1e-5).length > 1 || absComplex(d1) < 1e-14) {
      limits.push({ omega, text: "高重虚轴极点，按分支数值极限观察" }); return;
    }
    const residue = divComplex(evalPolynomial(model.numerator, p), d1);
    if (absComplex(residue) < 1e-12) return;
    const constant = divComplex(subComplex(evalPolynomial(derivativeNum, p),
      mulComplex(residue, mulComplex({ re: 0.5, im: 0 }, evalPolynomial(secondDen, p)))), d1);
    for (const side of (omega === 0 ? [1] : [-1, 1])) {
      const direction = { x: side * residue.im, y: -side * residue.re }, length = Math.hypot(direction.x, direction.y);
      const unit = { x: direction.x / length, y: direction.y / length };
      asymptotes.push({ origin: { x: gain * constant.re, y: gain * constant.im }, direction: unit, omega, side });
      limits.push({ omega, side, angle: Math.atan2(unit.y, unit.x) * 180 / Math.PI });
    }
  });
  const dc = originPole ? null : { omega: 0, ...mapAt({ re: 0, im: 0 }) };
  return { positive: positiveBranches.flatMap((b) => b.data), negative: negativeBranches.flatMap((b) => b.data),
    positiveBranches, negativeBranches, detours, contour, dc, asymptotes, limits, maxFrequency,
    imaginaryAxisPole: axisPoles.length > 0, imaginaryAxisPoles: axisPoles };
}
function nyquistAutoBounds(data, negative = false) {
  const branches = negative ? [...data.positiveBranches, ...data.negativeBranches] : data.positiveBranches;
  // Include a finite portion of every branch without letting singular endpoints dominate Auto.
  const minima = branches.map((branch) => Math.min(...branch.data.filter(finitePoint).map((p) => Math.hypot(p.x, p.y)))).filter(Number.isFinite);
  const dcRadius = finitePoint(data.dc) ? Math.hypot(data.dc.x, data.dc.y) : 0;
  const radius = Math.max(4, dcRadius * 1.15, ...minima.map((r) => r * 2));
  const focus = branches.flatMap((branch) => branch.data.filter((p) => finitePoint(p) && Math.hypot(p.x, p.y) <= radius));
  focus.push({ x: -1, y: 0 }, { x: 0, y: 0 });
  if (finitePoint(data.dc)) focus.push(data.dc);
  return {
    x: niceBounds(Math.min(...focus.map((p) => p.x)), Math.max(...focus.map((p) => p.x))),
    y: niceBounds(Math.min(...focus.map((p) => p.y)), Math.max(...focus.map((p) => p.y))),
  };
}
function nyquistWinding(data, critical = -1) {
  const points = data.contour || [...data.negative, ...(data.dc ? [data.dc] : []), ...data.positive];
  if (points.some((p) => !finitePoint(p))) return null;
  let angleSum = 0;
  for (let i = 0; i < points.length; i += 1) {
    const a = { re: points[i].x - critical, im: points[i].y }, next = points[(i + 1) % points.length], b = { re: next.x - critical, im: next.y };
    if (absComplex(a) < 1e-8 || absComplex(b) < 1e-8) return null;
    angleSum += Math.atan2(a.re * b.im - a.im * b.re, a.re * b.re + a.im * b.im);
  }
  return Math.round(angleSum / (2 * Math.PI)) || 0;
}
// Minimum-total-displacement matching keeps branches distinct at crossings.
function matchRoots(previous, roots) {
  const n = roots.length, u = new Array(n + 1).fill(0), v = u.slice(), p = u.slice(), way = u.slice();
  for (let i = 1; i <= n; i += 1) {
    p[0] = i; let j0 = 0;
    const min = new Array(n + 1).fill(Infinity), used = new Array(n + 1).fill(false);
    do {
      used[j0] = true;
      const i0 = p[j0]; let delta = Infinity, j1 = 0;
      for (let j = 1; j <= n; j += 1) if (!used[j]) {
        const cost = absComplex(subComplex(previous[i0 - 1], roots[j - 1])) - u[i0] - v[j];
        if (cost < min[j]) { min[j] = cost; way[j] = j0; }
        if (min[j] < delta) { delta = min[j]; j1 = j; }
      }
      for (let j = 0; j <= n; j += 1) { if (used[j]) { u[p[j]] += delta; v[j] -= delta; } else min[j] -= delta; }
      j0 = j1;
    } while (p[j0] !== 0);
    do { const j1 = way[j0]; p[j0] = p[j1]; j0 = j1; } while (j0 !== 0);
  }
  const assigned = [];
  for (let j = 1; j <= n; j += 1) assigned[p[j] - 1] = roots[j - 1];
  return assigned;
}
function rootLocusData(model, maxGain) {
  const info = modelCharacteristicsCached(model), breakpoints = findBreakawayPoints(model, info);
  const specialGains = breakpoints.map((s) => -evalPolynomial(model.denominator, { re: s, im: 0 }).re / evalPolynomial(model.numerator, { re: s, im: 0 }).re).filter((k) => k > 0 && k <= maxGain);
  const startGain = Math.min(1e-8, maxGain * 1e-10), count = Math.max(440, Math.ceil(Math.log10(maxGain / startGain) * 32));
  const gains = [0, ...Array.from({ length: count }, (_, i) => startGain * Math.pow(maxGain / startGain, i / (count - 1))),
    ...specialGains.flatMap((k) => [k * (1 - 1e-6), k, k * (1 + 1e-6)]).filter((k) => k <= maxGain)].sort((a, b) => a - b);
  const tracks = info.poles.map(() => []); let previous = info.poles;
  for (const gain of gains) {
    const sorted = matchRoots(previous, gain === 0 ? info.poles : polynomialRoots(closedLoopPolynomial(model, gain)));
    sorted.forEach((p, i) => tracks[i].push({ x: p.re, y: p.im, gain })); previous = sorted;
  }
  const difference = info.poles.length - info.zeros.length;
  const centroid = difference ? (info.poles.reduce((s, p) => s + p.re, 0) - info.zeros.reduce((s, p) => s + p.re, 0)) / difference : null;
  const angles = Array.from({ length: Math.max(0, difference) }, (_, i) => ((model.numerator[0] / model.denominator[0] > 0 ? 2 * i + 1 : 2 * i) * 180 / difference));
  const real = [...info.poles, ...info.zeros].filter((p) => Math.abs(p.im) < 1e-6).map((p) => p.re).sort((a, b) => a - b), edges = [-Infinity, ...new Set(real), Infinity];
  const intervals = edges.slice(0, -1).flatMap((a, i) => {
    const b = edges[i + 1], probe = !Number.isFinite(a) ? b - 1 : !Number.isFinite(b) ? a + 1 : (a + b) / 2, odd = real.filter((x) => x > probe).length % 2 === 1;
    return odd === (model.numerator[0] / model.denominator[0] > 0) ? [{ a, b }] : [];
  });
  const phaseSum = (p, list) => list.reduce((sum, q) => sum + Math.atan2(p.im - q.im, p.re - q.re) * 180 / Math.PI, 0);
  const angle = (a) => ((a % 360) + 360) % 360;
  const baseAngle = model.numerator[0] / model.denominator[0] > 0 ? 180 : 0;
  const departure = info.poles.filter((p) => Math.abs(p.im) > 1e-7).map((p) => ({
    point: p, angle: angle(baseAngle + phaseSum(p, info.zeros) - phaseSum(p, info.poles.filter((q) => q !== p))),
    repeated: info.poles.some((q) => q !== p && absComplex(subComplex(p, q)) < 1e-5),
  }));
  const arrival = info.zeros.filter((p) => Math.abs(p.im) > 1e-7).map((p) => ({
    point: p, angle: angle(baseAngle - phaseSum(p, info.zeros.filter((q) => q !== p)) + phaseSum(p, info.poles)),
    repeated: info.zeros.some((q) => q !== p && absComplex(subComplex(p, q)) < 1e-5),
  }));
  return { info, tracks, breakpoints, centroid, angles, intervals, departure, arrival, maxGain };
}


const lineChartState = new WeakMap();
const activeCharts = new Map();
const plotHint = "滚轮缩放 · 拖动平移 · 单击固定读数 · 双击恢复设置";
const plotNumber = (v) => Math.abs(v) < 1e-12 ? "0" : Number(v.toPrecision(5)).toString();
function frequencyTick(logValue) {
  const omega = Math.pow(10, logValue), exponent = Math.floor(logValue + 1e-10);
  const superscripts = { "-": "⁻", "0": "⁰", "1": "¹", "2": "²", "3": "³", "4": "⁴", "5": "⁵", "6": "⁶", "7": "⁷", "8": "⁸", "9": "⁹" };
  const power = String(exponent).split("").map((c) => superscripts[c]).join("");
  return (Math.abs(logValue - Math.round(logValue)) < 1e-8 ? "" : Number((omega / Math.pow(10, exponent)).toPrecision(3)) + "×") + "10" + power;
}
function clipSegment(a, b, domain) {
  if (!finitePoint(a) || !finitePoint(b)) return null;
  const dx = b.x - a.x, dy = b.y - a.y;
  let start = 0, end = 1;
  const ps = [-dx, dx, -dy, dy], qs = [a.x - domain.xMin, domain.xMax - a.x, a.y - domain.yMin, domain.yMax - a.y];
  for (let i = 0; i < 4; i += 1) {
    if (Math.abs(ps[i]) < 1e-30) { if (qs[i] < 0) return null; }
    else {
      const t = qs[i] / ps[i];
      if (ps[i] < 0) start = Math.max(start, t); else end = Math.min(end, t);
      if (start > end) return null;
    }
  }
  return [{ x: a.x + dx * start, y: a.y + dy * start }, { x: a.x + dx * end, y: a.y + dy * end }];
}
function radiusSegments(a, b, radius) {
  if (!radius) return [{ a, b, far: false }];
  const dx = b.x - a.x, dy = b.y - a.y, A = dx * dx + dy * dy, B = 2 * (a.x * dx + a.y * dy), C = a.x * a.x + a.y * a.y - radius * radius;
  const discriminant = B * B - 4 * A * C, ts = [0, 1];
  if (A > 0 && discriminant > 0) {
    for (const t of [(-B - Math.sqrt(discriminant)) / (2 * A), (-B + Math.sqrt(discriminant)) / (2 * A)]) if (t > 1e-12 && t < 1 - 1e-12) ts.push(t);
  }
  ts.sort((x, y) => x - y);
  const at = (t) => ({ x: a.x + dx * t, y: a.y + dy * t });
  return ts.slice(0, -1).map((t, i) => ({ a: at(t), b: at(ts[i + 1]), far: Math.hypot(at((t + ts[i + 1]) / 2).x, at((t + ts[i + 1]) / 2).y) > radius }));
}
function makeSvgLineChart(container, inputSeries, originalOptions) {
  const svg = container.querySelector("svg");
  const options = { ...originalOptions };
  let state = lineChartState.get(container);
  if (!state) { state = { attached: false, pinned: false, view: null }; lineChartState.set(container, state); }
  if (!options.interaction) {
    const key = options.viewKey || JSON.stringify([options.xMin, options.xMax, options.yMin, options.yMax]);
    if (state.key !== key) { state.view = null; state.pinned = false; }
    state.key = key; state.baseOptions = { ...options }; state.baseSeries = inputSeries;
  }
  const css = getComputedStyle(container);
  const width = Math.max(220, container.clientWidth - parseFloat(css.paddingLeft) - parseFloat(css.paddingRight));
  const height = Math.max(180, container.clientHeight - parseFloat(css.paddingTop) - parseFloat(css.paddingBottom));
  let left = 65, top = 20, plotWidth = width - 86, plotHeight = height - 70;
  let domain = state.view || {
    xMin: options.xMin, xMax: options.xMax,
    yMin: options.yMin, yMax: options.yMax,
  };
  if (!Number.isFinite(domain.xMin)) {
    const xs = inputSeries.flatMap((s) => s.data.filter(finitePoint).map((p) => p.x));
    domain = { ...domain, xMin: Math.min(...xs), xMax: Math.max(...xs) };
  }
  if (!Number.isFinite(domain.yMin) || !Number.isFinite(domain.yMax)) {
    const ys = inputSeries.flatMap((s) => s.data.filter(finitePoint).map((p) => p.y));
    const bounds = niceBounds(Math.min(...ys), Math.max(...ys));
    domain = { ...domain, yMin: bounds.min, yMax: bounds.max };
  }
  if (domain.xMax <= domain.xMin) domain.xMax = domain.xMin + 1;
  if (domain.yMax <= domain.yMin) domain.yMax = domain.yMin + 1;
  if (options.equalScale && options.expandAutoAxes) {
    const units = Math.max((domain.xMax - domain.xMin) / plotWidth, (domain.yMax - domain.yMin) / plotHeight);
    const cx = (domain.xMin + domain.xMax) / 2, cy = (domain.yMin + domain.yMax) / 2;
    domain = { xMin: cx - units * plotWidth / 2, xMax: cx + units * plotWidth / 2, yMin: cy - units * plotHeight / 2, yMax: cy + units * plotHeight / 2 };
    options.xStep = niceStep(domain.xMax - domain.xMin, Math.max(2, plotWidth / 75));
    options.yStep = niceStep(domain.yMax - domain.yMin, Math.max(2, plotHeight / 44));
  }
  if (options.equalScale) {
    const scale = Math.min(plotWidth / (domain.xMax - domain.xMin), plotHeight / (domain.yMax - domain.yMin));
    const pw = scale * (domain.xMax - domain.xMin), ph = scale * (domain.yMax - domain.yMin);
    left += (plotWidth - pw) / 2; top += (plotHeight - ph) / 2; plotWidth = pw; plotHeight = ph;
  }
  const sx = (x) => left + (x - domain.xMin) / (domain.xMax - domain.xMin) * plotWidth;
  const sy = (y) => top + (domain.yMax - y) / (domain.yMax - domain.yMin) * plotHeight;
  let series = options.sample ? options.sample(domain) : inputSeries;
  state.options = options; state.domain = domain; state.series = series; state.layout = { width, height, left, top, plotWidth, plotHeight, sx, sy };
  state.pinned = false;
  const fmtX = options.logX ? frequencyTick : (options.formatX || plotNumber);
  const fmtY = options.formatY || plotNumber;
  let xTicks;
  if (options.logX && domain.xMax - domain.xMin >= 1) {
    const stride = Math.max(1, Math.ceil((domain.xMax - domain.xMin) / Math.max(2, plotWidth / 75)));
    xTicks = [];
    for (let x = Math.ceil(domain.xMin / stride) * stride; x <= domain.xMax + 1e-8; x += stride) xTicks.push(x);
  } else {
    const step = state.view ? niceStep(domain.xMax - domain.xMin, Math.max(2, plotWidth / 75)) : options.xStep;
    xTicks = axisTicks(domain.xMin, domain.xMax, step, Boolean(state.view) || options.expandAutoAxes || options.xStep === undefined);
  }
  const yTicks = axisTicks(domain.yMin, domain.yMax, state.view ? niceStep(domain.yMax - domain.yMin, Math.max(2, plotHeight / 44)) : options.yStep, Boolean(state.view) || options.expandAutoAxes || options.yStep === undefined);
  const id = container.id + "-clip", endX = left + plotWidth, endY = top + plotHeight;
  let markup = '<defs><clipPath id="' + id + '"><rect x="' + left + '" y="' + top + '" width="' + plotWidth + '" height="' + plotHeight + '"/></clipPath></defs>';
  const xStride = Math.max(1, Math.ceil(xTicks.length / Math.max(2, plotWidth / 64)));
  const yStride = Math.max(1, Math.ceil(yTicks.length / Math.max(2, plotHeight / 27)));
  xTicks.forEach((value, i) => {
    markup += '<line class="chart-grid" x1="' + sx(value) + '" x2="' + sx(value) + '" y1="' + top + '" y2="' + endY + '"/>';
    if (i % xStride === 0) markup += '<text class="chart-tick" x="' + sx(value) + '" y="' + (endY + 20) + '" text-anchor="middle">' + fmtX(value) + '</text>';
  });
  yTicks.forEach((value, i) => {
    markup += '<line class="chart-grid" x1="' + left + '" x2="' + endX + '" y1="' + sy(value) + '" y2="' + sy(value) + '"/>';
    if (i % yStride === 0) markup += '<text class="chart-tick" x="' + (left - 8) + '" y="' + (sy(value) + 4) + '" text-anchor="end">' + fmtY(value) + '</text>';
  });
  markup += '<rect class="plot-frame" x="' + left + '" y="' + top + '" width="' + plotWidth + '" height="' + plotHeight + '"/><g clip-path="url(#' + id + ')">';
  if (domain.xMin <= 0 && domain.xMax >= 0 && !options.logX) markup += '<line class="axis-zero" x1="' + sx(0) + '" x2="' + sx(0) + '" y1="' + top + '" y2="' + endY + '"/>';
  if (domain.yMin <= 0 && domain.yMax >= 0) markup += '<line class="axis-zero" x1="' + left + '" x2="' + endX + '" y1="' + sy(0) + '" y2="' + sy(0) + '"/>';
  (options.horizontals || (options.reference === undefined ? [] : [options.reference])).forEach((y) => {
    if (y >= domain.yMin && y <= domain.yMax) markup += '<line class="chart-reference" x1="' + left + '" x2="' + endX + '" y1="' + sy(y) + '" y2="' + sy(y) + '"/>';
  });
  (options.verticals || []).forEach((line) => {
    if (line.x >= domain.xMin && line.x <= domain.xMax) markup += '<line class="chart-annotation-line" x1="' + sx(line.x) + '" x2="' + sx(line.x) + '" y1="' + top + '" y2="' + endY + '"/>';
  });
  (options.rays || []).forEach((ray) => {
    const reach = 4 * (Math.hypot(domain.xMax - domain.xMin, domain.yMax - domain.yMin) + Math.hypot(ray.origin.x, ray.origin.y));
    const cut = clipSegment(ray.origin, { x: ray.origin.x + ray.direction.x * reach, y: ray.origin.y + ray.direction.y * reach }, domain);
    if (cut) markup += '<path class="plot-asymptote" d="M' + sx(cut[0].x) + ',' + sy(cut[0].y) + ' L' + sx(cut[1].x) + ',' + sy(cut[1].y) + '"/>';
  });
  state.visibleSegments = [];
  series.forEach((item) => {
    let normal = "", far = "", travelled = 0, arrows = 0;
    for (let i = 1; i < item.data.length; i += 1) {
      const a = item.data[i - 1], b = item.data[i];
      if (!finitePoint(a) || !finitePoint(b)) continue;
      for (const segment of radiusSegments(a, b, options.radialThreshold)) {
        const clipped = clipSegment(segment.a, segment.b, domain);
        if (!clipped) continue;
        const p = { x: sx(clipped[0].x), y: sy(clipped[0].y) }, q = { x: sx(clipped[1].x), y: sy(clipped[1].y) };
        const d = 'M' + p.x.toFixed(3) + ',' + p.y.toFixed(3) + 'L' + q.x.toFixed(3) + ',' + q.y.toFixed(3);
        if (segment.far) far += d; else normal += d;
        if (item.inspectable !== false) state.visibleSegments.push({ a, b, name: item.name || "曲线", p, q, className: item.className });
        const length = Math.hypot(q.x - p.x, q.y - p.y); travelled += length;
        if (item.arrows && length > 0.1 && travelled > 65 && arrows < 4) {
          const t = 0.6, x = p.x + (q.x - p.x) * t, y = p.y + (q.y - p.y) * t;
          const angle = Math.atan2(q.y - p.y, q.x - p.x) * 180 / Math.PI;
          markup += '<path class="direction-arrow ' + (item.className || "") + '" transform="translate(' + x + ' ' + y + ') rotate(' + angle + ')" d="M-7,-4 L0,0 L-7,4"/>';
          travelled = -60; arrows += 1;
        }
      }
    }
    const cls = item.className || "chart-data-line";
    markup += '<path class="' + cls + '" d="' + normal + '"/><path class="' + cls + ' far-tail" d="' + far + '"/>';
  });
  (options.points || []).forEach((point) => {
    if (point.x < domain.xMin || point.x > domain.xMax || point.y < domain.yMin || point.y > domain.yMax) return;
    const x = sx(point.x), y = sy(point.y);
    if (point.symbol === "cross") markup += '<path class="pole-symbol" d="M' + (x - 5) + ',' + (y - 5) + 'l10,10 M' + (x - 5) + ',' + (y + 5) + 'l10,-10"/>';
    else markup += '<circle class="' + (point.symbol === "zero" ? "zero-symbol" : point.className || "chart-annotation-point") + '" cx="' + x + '" cy="' + y + '" r="' + (point.radius || 4) + '"/>';
  });
  markup += '</g><text class="chart-axis-title" x="' + (left + plotWidth / 2) + '" y="' + (height - 6) + '" text-anchor="middle">' + (options.xLabel || "t (s)") + '</text><text class="chart-axis-title" transform="translate(14 ' + (top + plotHeight / 2) + ') rotate(-90)" text-anchor="middle">' + (options.yLabel || "y(t)") + '</text>';
  svg.setAttribute("viewBox", "0 0 " + width + " " + height); svg.innerHTML = markup;
  svg.dataset.xMin = domain.xMin; svg.dataset.xMax = domain.xMax; svg.dataset.yMin = domain.yMin; svg.dataset.yMax = domain.yMax;
  activeCharts.set(container.id, container);
  attachChartInteractions(container, state);
}
function attachChartInteractions(container, state) {
  const svg = container.querySelector("svg");
  const readout = document.getElementById("cursor-readout");
  if (state.attached) return;
  state.attached = true;
  const coordinate = (event) => {
    const point = svg.createSVGPoint(); point.x = event.clientX; point.y = event.clientY;
    const matrix = svg.getScreenCTM();
    return matrix ? point.matrixTransform(matrix.inverse()) : { x: 0, y: 0 };
  };
  const inPlot = (pos) => pos.x >= state.layout.left && pos.x <= state.layout.left + state.layout.plotWidth && pos.y >= state.layout.top && pos.y <= state.layout.top + state.layout.plotHeight;
  const redraw = (domain, link = true) => {
    state.view = domain; state.pinned = false;
    makeSvgLineChart(container, state.baseSeries, { ...state.baseOptions, interaction: true });
    if (link && state.options.linkGroup) activeCharts.forEach((peer) => {
      const other = lineChartState.get(peer);
      if (peer !== container && other?.options.linkGroup === state.options.linkGroup && !peer.hidden) {
        other.view = domain ? { ...other.domain, xMin: domain.xMin, xMax: domain.xMax } : null;
        makeSvgLineChart(peer, other.baseSeries, { ...other.baseOptions, interaction: true });
      }
    });
    const d = state.domain;
    readout.textContent = (state.options.name || "图形") + " · 当前范围：横轴 " + (state.options.logX ? plotNumber(Math.pow(10, d.xMin)) : plotNumber(d.xMin)) + "～" + (state.options.logX ? plotNumber(Math.pow(10, d.xMax)) : plotNumber(d.xMax)) + "；纵轴 " + plotNumber(d.yMin) + "～" + plotNumber(d.yMax) + " · " + plotHint;
  };
  const inspect = (event) => {
    const pos = coordinate(event); if (!inPlot(pos)) return false;
    let nearest = null, best = Infinity;
    const { sx, sy } = state.layout;
    state.visibleSegments.forEach((segment) => {
      const dx = segment.q.x - segment.p.x, dy = segment.q.y - segment.p.y;
      const t = Math.max(0, Math.min(1, ((pos.x - segment.p.x) * dx + (pos.y - segment.p.y) * dy) / (dx * dx + dy * dy || 1)));
      const px = segment.p.x + t * dx, py = segment.p.y + t * dy;
      const distance = Math.hypot(px - pos.x, py - pos.y);
      if (distance >= best) return;
      const d = state.domain, l = state.layout;
      const x = d.xMin + (px - l.left) / l.plotWidth * (d.xMax - d.xMin);
      const y = d.yMax - (py - l.top) / l.plotHeight * (d.yMax - d.yMin);
      const fullDx = segment.b.x - segment.a.x, fullDy = segment.b.y - segment.a.y;
      const ratio = Math.abs(fullDx) > Math.abs(fullDy) ? (x - segment.a.x) / (fullDx || 1) : (y - segment.a.y) / (fullDy || 1);
      const point = { x, y };
      for (const field of ["omega", "gain"]) if (Number.isFinite(segment.a[field]) && Number.isFinite(segment.b[field])) point[field] = segment.a[field] + ratio * (segment.b[field] - segment.a[field]);
      nearest = { ...point, name: segment.name, approximate: true }; best = distance;
    });
    (state.options.points || []).forEach((point) => {
      const distance = Math.hypot(sx(point.x) - pos.x, sy(point.y) - pos.y);
      if (distance < 12 && distance <= best + 3) { best = distance; nearest = { ...point, name: point.name || "特征点" }; }
    });
    if (!nearest) return false;
    readout.textContent = state.options.readout ? state.options.readout(nearest) : (nearest.name + " · x ≈ " + plotNumber(nearest.x) + " · y ≈ " + plotNumber(nearest.y));
    svg.querySelector(".chart-crosshair")?.remove();
    const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
    group.setAttribute("class", "chart-crosshair"); group.setAttribute("pointer-events", "none");
    const x = sx(nearest.x), y = sy(nearest.y), l = state.layout;
    group.innerHTML = '<line class="crosshair-x" x1="' + x + '" x2="' + x + '" y1="' + l.top + '" y2="' + (l.top + l.plotHeight) + '"/><line class="crosshair-y" x1="' + l.left + '" x2="' + (l.left + l.plotWidth) + '" y1="' + y + '" y2="' + y + '"/><circle class="crosshair-point" cx="' + x + '" cy="' + y + '" r="4"/>';
    svg.appendChild(group); return true;
  };
  svg.addEventListener("wheel", (event) => {
    const pos = coordinate(event); if (!inPlot(pos)) return;
    event.preventDefault();
    const d = state.domain, l = state.layout, factor = event.deltaY < 0 ? 0.8 : 1.25;
    const fx = (pos.x - l.left) / l.plotWidth, fy = (pos.y - l.top) / l.plotHeight;
    const dx = (d.xMax - d.xMin) * factor, dy = (d.yMax - d.yMin) * factor;
    if (dx < 1e-9 || dy < 1e-9 || dx > 1e12 || dy > 1e12) return;
    const cx = d.xMin + fx * (d.xMax - d.xMin), cy = d.yMax - fy * (d.yMax - d.yMin);
    const next = { xMin: cx - fx * dx, xMax: cx + (1 - fx) * dx, yMin: cy - (1 - fy) * dy, yMax: cy + fy * dy };
    if (state.options.logX && (next.xMin < -12 || next.xMax > 12)) return;
    redraw(next);
  }, { passive: false });
  svg.addEventListener("pointerdown", (event) => {
    if (event.button !== 0) return;
    const pos = coordinate(event); if (!inPlot(pos)) return;
    state.drag = { pos, domain: { ...state.domain }, moved: false }; svg.setPointerCapture(event.pointerId);
  });
  svg.addEventListener("pointermove", (event) => {
    if (!state.drag) { if (!state.pinned) inspect(event); return; }
    const pos = coordinate(event), drag = state.drag, d = drag.domain, l = state.layout;
    if (Math.hypot(pos.x - drag.pos.x, pos.y - drag.pos.y) < 4 && !drag.moved) return;
    drag.moved = true;
    const dx = (pos.x - drag.pos.x) / l.plotWidth * (d.xMax - d.xMin), dy = (pos.y - drag.pos.y) / l.plotHeight * (d.yMax - d.yMin);
    const next = { xMin: d.xMin - dx, xMax: d.xMax - dx, yMin: d.yMin + dy, yMax: d.yMax + dy };
    if (!state.options.logX || (next.xMin >= -12 && next.xMax <= 12)) redraw(next);
  });
  svg.addEventListener("pointerup", (event) => {
    state.justDragged = state.drag?.moved; state.drag = null;
    if (svg.hasPointerCapture(event.pointerId)) svg.releasePointerCapture(event.pointerId);
  });
  svg.addEventListener("pointercancel", () => { state.drag = null; state.justDragged = true; });
  svg.addEventListener("click", (event) => {
    if (state.justDragged) { state.justDragged = false; return; }
    if (state.pinned) { state.pinned = false; svg.querySelector(".chart-crosshair")?.remove(); readout.textContent = plotHint; }
    else state.pinned = inspect(event);
  });
  svg.addEventListener("dblclick", (event) => { event.preventDefault(); redraw(null); });
}


const EXAMPLES = {
  second: { numerator: "25", denominator: "1, 6, 25", duration: 8 },
  first: { numerator: "2", denominator: "1, 2", duration: 5 },
  integrator: { numerator: "1", denominator: "1, 2, 0", duration: 12 },
  notebook: { numerator: "5000", denominator: "1, 5, 25, 125, 0", duration: 8 },
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
      { label: "上升时间", value: !Number.isFinite(metrics.riseTime) ? null : metrics.riseTime.toFixed(3), unit: "s" },
      { label: "峰值时间", value: !Number.isFinite(metrics.peakTime) ? null : metrics.peakTime.toFixed(3), unit: "s" },
      { label: "超调量", value: !Number.isFinite(metrics.overshoot) ? null : metrics.overshoot.toFixed(2), unit: "%", accent: true },
      { label: "调节时间", value: !Number.isFinite(metrics.settlingTime) ? null : metrics.settlingTime.toFixed(3), unit: "s" },
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

  function fixedAxis(prefix) {
    const min = Number($("#" + prefix + "-min").value), max = Number($("#" + prefix + "-max").value), step = Number($("#" + prefix + "-step").value);
    if (["min", "max", "step"].some((s) => !$("#" + prefix + "-" + s).value.trim())) throw new Error("请填写完整的坐标最小值、最大值和每格步长。");
    axisTicks(min, max, step);
    return { min, max, step };
  }

  function syncPlotControls() {
    const frequency = currentView === "frequency", bode = frequency && ui.frequencyMode.value === "bode";
    const nyquist = currentView === "stability" || (frequency && !bode);
    $("#bode-scale-settings").hidden = !bode;
    $("#nyquist-scale-settings").hidden = !nyquist;
    $("#complex-plot-help").hidden = !(nyquist || currentView === "root");
    $("#frequency-custom").hidden = ui.frequencyRange.value !== "custom";
    ["magnitude", "phase", "nyquist"].forEach((name) => {
      $("#" + name + "-scale-fields").hidden = $("#" + name + "-scale-mode").value !== "fixed";
    });
    ui.bodeAnalysis.hidden = !frequency; ui.rootAnalysis.hidden = currentView !== "root";
    $("#parameter-details").hidden = !(frequency || currentView === "root");
  }

  function clearPlotViews() {
    activeCharts.forEach((container) => { const state = lineChartState.get(container); if (state) { state.view = null; state.key = null; } });
    $("#cursor-readout").textContent = plotHint;
  }

  function plotNyquist(container, model, gain, data) {
    const negative = $("#nyquist-negative").checked, detours = $("#nyquist-detours").checked;
    const series = data.positiveBranches.map((branch) => ({ ...branch, className: "nyquist-positive", arrows: true }));
    if (negative) series.push(...data.negativeBranches.map((branch) => ({ ...branch, className: "nyquist-negative", arrows: true })));
    if (detours) data.detours.forEach((arc) => {
      series.push({ ...arc, className: "nyquist-detour", arrows: true, inspectable: false });
      if (negative) series.push({ name: arc.name, data: arc.data.slice().reverse().map((p) => ({ x: p.x, y: -p.y })), className: "nyquist-detour", arrows: true, inspectable: false });
    });
    const { x: xAuto, y: yAuto } = nyquistAutoBounds(data, negative);
    const fixed = $("#nyquist-scale-mode").value === "fixed";
    const x = fixed ? fixedAxis("nyquist-x") : xAuto, y = fixed ? fixedAxis("nyquist-y") : yAuto;
    const rays = ui.asymptoteToggle.checked ? data.asymptotes.slice() : [];
    if (negative) rays.push(...rays.map((r) => ({ origin: { x: r.origin.x, y: -r.origin.y }, direction: { x: r.direction.x, y: -r.direction.y } })));
    const points = [{ x: -1, y: 0, name: "临界点 (−1, 0)", className: "critical-point", radius: 5 }];
    if (data.dc && finitePoint(data.dc)) points.push({ ...data.dc, name: "直流点 ω=0", className: "dc-point" });
    makeSvgLineChart(container, series, {
      name: "Nyquist", xMin: x.min, xMax: x.max, yMin: y.min, yMax: y.max, xStep: x.step, yStep: y.step,
      xLabel: "Re[L(jω)]", yLabel: "Im[L(jω)]", equalScale: true, expandAutoAxes: !fixed, radialThreshold: FAR_RADIUS, points, rays,
      viewKey: JSON.stringify([model, gain, fixed, x, y, negative]),
      readout: (point) => point.name + " · Re ≈ " + plotNumber(point.x) + " · Im ≈ " + plotNumber(point.y) + " · |L| ≈ " + plotNumber(Math.hypot(point.x, point.y)) +
        (Number.isFinite(point.omega) ? " · ω ≈ " + plotNumber(point.omega) + " rad/s · f ≈ " + plotNumber(point.omega / (2 * Math.PI)) + " Hz" : ""),
    });
  }

  function nyquistLimitText(data) {
    return data.limits.map((limit) => "ω→" + plotNumber(limit.omega) + (limit.side === -1 ? "⁻" : "⁺") + "：" +
      (limit.text || "|L|→∞，方向 " + limit.angle.toFixed(2) + "°")).concat(["ω→∞：L→0"]).join("；");
  }

  function drawFrequency(model) {
    const decade = Number(ui.frequencyRange.value);
    const minW = ui.frequencyRange.value === "custom" ? Number($("#frequency-min").value) : Math.pow(10, -decade);
    const maxW = ui.frequencyRange.value === "custom" ? Number($("#frequency-max").value) : Math.pow(10, decade);
    if (!(minW >= 1e-12 && maxW <= 1e12 && maxW > minW)) throw new Error("频率范围应满足 10⁻¹² ≤ ω 最小值 < ω 最大值 ≤ 10¹² rad/s。");
    const data = frequencySamples(model, minW, maxW), finite = data.filter((p) => Number.isFinite(p.magnitude) && Number.isFinite(p.phase));
    if (!finite.length) throw new Error("此频率范围内没有有限频率响应。");
    const roots = modelCharacteristicsCached(model);
    const gainCrossovers = estimateCrossovers(data, "magnitude", 0);
    const marginCandidates = gainCrossovers.map((omega) => {
      const phase = interpolateFrequencyField(data, omega, "phase");
      return { omega, phase, margin: ((phase + 180 + 540) % 360 + 360) % 360 - 180 };
    });
    const phasePoint = marginCandidates.reduce((best, p) => !best || Math.abs(p.margin) < Math.abs(best.margin) ? p : best, null);
    const pMin = Math.min(...finite.map((p) => p.phase)), pMax = Math.max(...finite.map((p) => p.phase));
    const gainCandidates = [];
    for (let phase = 180 + 360 * Math.ceil((pMin - 180) / 360); phase <= pMax; phase += 360) {
      estimateCrossovers(data, "phase", phase).forEach((omega) => gainCandidates.push({ omega, phase, margin: -interpolateFrequencyField(data, omega, "magnitude") }));
    }
    const gainPoint = gainCandidates.reduce((best, p) => !best || Math.abs(p.margin) < Math.abs(best.margin) ? p : best, null);
    const phaseMarginText = phasePoint ? phasePoint.margin.toFixed(2) + "°" : "范围内未检出";
    const gainMarginText = gainPoint ? gainPoint.margin.toFixed(2) + " dB" : "范围内未检出";
    setParameterCards([
      { label: "模型首项系数比", value: plotNumber(roots.gain) }, { label: "型别", value: roots.type, unit: "型" },
      { label: "相对阶", value: roots.relativeOrder, unit: "阶" },
      { label: "相位裕度", value: phaseMarginText, accent: true }, { label: "幅值裕度", value: gainMarginText, accent: true },
      { label: "0 dB 交点", value: gainCrossovers.length, unit: "个" },
    ], "频域特性 · 数值与坐标读数");
    const nyquist = ui.frequencyMode.value === "nyquist";
    $("#frequency-title").textContent = nyquist ? "Nyquist 坐标图 · L(s)=G(s)" : "Bode 图";
    $("#frequency-kicker").textContent = nyquist ? "NYQUIST PLOT" : "FREQUENCY RESPONSE";
    ui.bodeChart.hidden = nyquist; ui.phaseChart.hidden = nyquist; ui.nyquistChart.hidden = !nyquist;
    const corners = dedupeNumbers(roots.corners.map((p) => p.omega));
    const intervalEdges = [minW, ...corners.filter((w) => w > minW && w < maxW), maxW];
    const slopes = intervalEdges.slice(0, -1).map((start, i) => plotNumber(start) + "～" + plotNumber(intervalEdges[i + 1]) + " rad/s：" + asymptoticSlopeDbDecade(roots, Math.sqrt(start * intervalEdges[i + 1])) + " dB/dec");
    const query = Number(ui.bodeQueryValue.value);
    ui.bodeQueryResult.textContent = Number.isFinite(query) ? evaluateQuery(data, ui.bodeQueryMode.value, query) : "请输入有效数值。";
    const rows = [
      ["转折频率 ω", corners.length ? corners.map(plotNumber).join("、") + " rad/s" : "无"],
      ["全部 0 dB 交点", gainCrossovers.length ? gainCrossovers.map(plotNumber).join("、") + " rad/s" : "当前扫描范围内无"],
      ["相位裕度 / ωc", phasePoint ? phaseMarginText + " / " + plotNumber(phasePoint.omega) + " rad/s" : "当前扫描范围内无"],
      ["幅值裕度 / ωπ", gainPoint ? gainMarginText + " / " + plotNumber(gainPoint.omega) + " rad/s" : "未检出不等于全频段无穷大"],
      ["各段渐近斜率", slopes.join("；")],
      ["参数查询", ui.bodeQueryResult.textContent],
      ["扫描范围", plotNumber(minW) + "～" + plotNumber(maxW) + " rad/s；缩放只改变视图，裕度按此扫描范围计算"],
    ];
    if (nyquist) {
      const nyquist = nyquistData(model, 1);
      plotNyquist(ui.nyquistChart, model, 1, nyquist);
      rows.push(["极限方向", nyquistLimitText(nyquist)], ["绘图说明", "L=G，K=1；红点 (−1,0)。模值超过 10000 的曲线为虚线；灰虚线为渐近线，紫虚线为真实绕行映射。"]);
      $("#frequency-note").textContent = "精确坐标、实虚轴等比例。绿色为正频率，蓝色为负频率；箭头按 ω 增大。无穷远绕行映射可能在当前视野之外。";
    } else {
      const sharedReadout = (point) => {
        const omega = Math.pow(10, point.x), value = frequencyResponse(model.numerator, model.denominator, omega);
        const phase = frequencySamples(model, omega * (1 - 1e-6), omega * (1 + 1e-6), 3)[1]?.phase;
        return point.name + " · ω = " + plotNumber(omega) + " rad/s · f = " + plotNumber(omega / (2 * Math.PI)) + " Hz · 实际幅值 = " + (20 * Math.log10(absComplex(value))).toFixed(3) + " dB · 相位 = " + Number(phase).toFixed(2) + "°" + (point.name === "幅值渐近线" ? " · 渐近值 ≈ " + point.y.toFixed(3) + " dB" : "");
      };
      for (const [field, prefix, chart, label, reference] of [
        ["magnitude", "magnitude", ui.bodeChart, "幅值 |G| (dB)", 0],
        ["phase", "phase", ui.phaseChart, "相角 ∠G (°)", -180],
      ]) {
        const fixed = $("#" + prefix + "-scale-mode").value === "fixed";
        const visibleValues = finite.map((p) => p[field]);
        const bounds = fixed ? fixedAxis(prefix) : niceBounds(Math.min(...visibleValues), Math.max(...visibleValues), reference);
        const sample = (domain) => {
          const points = frequencySamples(model, Math.pow(10, domain.xMin), Math.pow(10, domain.xMax));
          const series = [{ name: field === "magnitude" ? "幅频曲线" : "相频曲线", data: points.map((p) => ({ x: Math.log10(p.omega), y: p[field], omega: p.omega })) }];
          if (field === "magnitude" && ui.asymptoteToggle.checked) series.push({ name: "幅值渐近线", data: points.map((p) => ({ x: Math.log10(p.omega), y: asymptoticMagnitudeDb(model, roots, p.omega) })), className: "asymptote-line" });
          return series;
        };
        makeSvgLineChart(chart, [], {
          name: field === "magnitude" ? "Bode 幅频" : "Bode 相频",
          xMin: Math.log10(minW), xMax: Math.log10(maxW), yMin: bounds.min, yMax: bounds.max, yStep: bounds.step,
          xLabel: "角频率 ω (rad/s)", yLabel: label, logX: true, linkGroup: "bode", sample, readout: sharedReadout,
          horizontals: ui.asymptoteToggle.checked ? [reference] : [],
          viewKey: JSON.stringify([model, minW, maxW, bounds, fixed]),
        });
      }
      $("#frequency-note").textContent = "幅频、相频横轴联动；Auto 自适应，固定刻度按设置绘制。滚轮缩放后重新采样，双击恢复设置。所有分析数值均在参数区。";
    }
    ui.bodeAnalysis.innerHTML = '<div class="analysis-grid">' + rows.map(([label, value]) => "<span>" + label + "</span><b>" + value + "</b>").join("") + "</div>";
  }

  function drawRoot(model) {
    const maxGain = Number(ui.gainMax.value);
    if (!Number.isFinite(maxGain) || maxGain <= 0 || maxGain > 1e30) throw new Error("K 上限需大于 0 且不超过 10³⁰；它与模值 10000 的显示阈值不同。");
    const data = rootLocusData(model, maxGain), { info, tracks, centroid, angles, breakpoints } = data;
    const points = [...info.poles.map((p) => ({ x: p.re, y: p.im, symbol: "cross", name: "开环极点", gain: 0 })),
      ...info.zeros.map((p) => ({ x: p.re, y: p.im, symbol: "zero", name: "开环零点" }))];
    const finite = tracks.flat().filter((p) => finitePoint(p) && Math.hypot(p.x, p.y) <= FAR_RADIUS);
    const xs = finite.concat(points).map((p) => p.x), ys = finite.concat(points).map((p) => p.y);
    const x = niceBounds(Math.min(0, ...xs), Math.max(0, ...xs)), y = niceBounds(Math.min(-1, ...ys), Math.max(1, ...ys));
    const rays = ui.asymptoteToggle.checked && centroid !== null ? angles.map((angle) => ({ origin: { x: centroid, y: 0 }, direction: { x: Math.cos(angle * Math.PI / 180), y: Math.sin(angle * Math.PI / 180) } })) : [];
    makeSvgLineChart(ui.rootChart, tracks.map((track, i) => ({ name: "根轨迹分支 " + (i + 1), data: track, className: "root-track", arrows: true })), {
      name: "根轨迹", xMin: x.min, xMax: x.max, yMin: y.min, yMax: y.max, xStep: x.step, yStep: y.step,
      xLabel: "实轴 Re(s)", yLabel: "虚轴 Im(s)", equalScale: true, expandAutoAxes: true, radialThreshold: FAR_RADIUS, rays, points,
      viewKey: JSON.stringify([model, maxGain]),
      readout: (p) => p.name + " · s ≈ " + formatComplex({ re: p.x, im: p.y }, 5) + " · |s| ≈ " + plotNumber(Math.hypot(p.x, p.y)) + (Number.isFinite(p.gain) ? " · K ≈ " + plotNumber(p.gain) : ""),
    });
    const imaginary = findImaginaryCrossings(model);
    const equation = formatPolynomial(model.denominator) + " + K(" + formatPolynomial(model.numerator) + ") = 0";
    const angleText = (list) => list.length ? list.map((p) => formatComplex(p.point) + "：" + (p.repeated ? "重根，需按多分支角条件判断" : p.angle.toFixed(2) + "°")).join("；") : "无复数点";
    const intervalText = (x) => x === Infinity ? "+∞" : x === -Infinity ? "−∞" : plotNumber(x);
    const rows = [
      ["传递函数", "G(s)=(" + formatPolynomial(model.numerator) + ")/(" + formatPolynomial(model.denominator) + ")"], ["特征方程", equation],
      ["规则 1 · 分支数量", info.poles.length + " 条；K≥0"], ["规则 2 · 起点与终点", "起点 ×：" + info.poles.map((p) => formatComplex(p)).join("、") + "；终点 ○：" + (info.zeros.map((p) => formatComplex(p)).join("、") || "无有限零点") + "；" + angles.length + " 条趋于无穷远"],
      ["规则 3 · 对称性", "实系数系统，关于实轴共轭对称"], ["规则 4 · 实轴区段", data.intervals.map((r) => "(" + intervalText(r.a) + ", " + intervalText(r.b) + ")").join("；") || "无"],
      ["规则 5 · 渐近线", centroid === null ? "无" : "交点 s=" + plotNumber(centroid) + "；角度 " + angles.map((a) => a.toFixed(2) + "°").join("、")],
      ["规则 6 · 分离/汇合", breakpoints.map((s) => "s=" + plotNumber(s) + "，K=" + plotNumber(-evalPolynomial(model.denominator, { re: s, im: 0 }).re / evalPolynomial(model.numerator, { re: s, im: 0 }).re)).join("；") || "未找到满足 K>0 的实轴点"],
      ["规则 7 · 出射/入射", "出射：" + angleText(data.departure) + "；入射：" + angleText(data.arrival)],
      ["规则 8 · 穿越 jω", imaginary.map((p) => "K=" + plotNumber(p.gain) + "，ω=" + plotNumber(p.omega) + " rad/s").join("；") || "数值搜索范围内未找到"],
      ["远端显示", "|s|>10000 的实际轨迹为红色虚线；灰色虚线仅表示理论渐近方向。计算范围为 0≤K≤" + plotNumber(maxGain)],
    ];
    ui.rootAnalysis.innerHTML = '<div class="analysis-grid">' + rows.map(([label, value]) => "<span>" + label + "</span><b>" + value + "</b>").join("") + "</div>";
    setParameterCards([
      { label: "阶数差 n−m", value: angles.length }, { label: "渐近线交点", value: centroid === null ? "无" : plotNumber(centroid) },
      { label: "分支数", value: info.poles.length }, { label: "分离/汇合点", value: breakpoints.length },
      { label: "远端模值阈值", value: FAR_RADIUS }, { label: "K 计算上限", value: plotNumber(maxGain) },
    ], "根轨迹 · 参数与八条绘图规则");
    $("#root-note").textContent = "箭头表示 K 增大方向；× 为极点，○ 为零点。虚线按到原点的距离判断，不按 K 判断。详细规则在上方参数区展开。";
  }

  function drawStability(model) {
    const gain = Number(ui.stabilityGain.value);
    if (!Number.isFinite(gain) || gain <= 0 || gain > 10000) throw new Error("反馈增益 K 需大于 0 且不超过 10000。");
    const polynomial = closedLoopPolynomial(model, gain), table = routhTable(polynomial), roots = polynomialRoots(polynomial);
    const axis = roots.filter((p) => Math.abs(p.re) < 1e-7);
    const repeated = axis.some((p, i) => axis.some((q, j) => i !== j && absComplex(subComplex(p, q)) < 1e-5));
    const unstable = table.signChanges > 0 || roots.some((p) => p.re > 1e-6) || repeated;
    const status = unstable ? "不稳定" : axis.length ? "临界稳定（非渐近稳定）" : "稳定";
    $("#stability-gain-label").textContent = plotNumber(gain);
    $("#stability-summary").innerHTML = '<div class="stability-status' + (unstable || axis.length ? " fail" : "") + '"><span class="status-dot-large"></span>劳斯判据：' + status + "（首列变号 " + table.signChanges + ' 次）</div><p class="scale-help">特征方程：' + formatPolynomial(polynomial) + "=0</p>";
    let html = '<table><thead><tr><th>s 次方</th>';
    for (let c = 0; c < table.table[0].length; c += 1) html += "<th>列 " + (c + 1) + "</th>";
    html += "</tr></thead><tbody>";
    table.table.forEach((row, i) => {
      html += "<tr><td>s<sup>" + (table.order - i) + "</sup></td>";
      row.forEach((v) => { html += '<td class="' + (v < 0 ? "negative" : v === 0 ? "zero" : "") + '">' + plotNumber(v) + "</td>"; });
      html += "</tr>";
    });
    html += "</tbody></table>";
    if (table.specialRows.length) html += '<p class="scale-help">整行零：已用上一行辅助多项式的导数替换。</p>';
    if (table.epsilonRows.length) html += '<p class="scale-help">首项为零：计算以 ε=10⁻⁹ 近似 ε→0⁺；表中的极大数来自此替换，稳定性另用闭环根校验。</p>';
    $("#routh-table").innerHTML = html;
    const data = nyquistData(model, gain), winding = nyquistWinding(data);
    const P = modelCharacteristicsCached(model).poles.filter((p) => p.re > 1e-7).length;
    const calculatedZ = winding === null ? null : P - winding;
    const reliable = !axis.length && calculatedZ !== null && calculatedZ === table.signChanges;
    const nyquistStatus = axis.length ? "经过临界点，单独判断虚轴根" : reliable ? (calculatedZ === 0 ? "稳定" : "不稳定") : "计数需复核";
    plotNyquist(ui.stabilityNyquistChart, model, gain, data);
    const rows = [
      ["环路传递函数", "L(s)=" + plotNumber(gain) + "·G(s)"], ["临界点", "(−1, 0)"],
      ["开环右半平面极点 P", P], ["逆时针环绕数 N", reliable ? winding : "不报告未经校验的计数"],
      ["闭环右半平面根 Z", reliable ? calculatedZ + "（Z=P−N）" : "见劳斯表与闭环极点"],
      ["Nyquist 判据", nyquistStatus], ["闭环极点", roots.map((p) => formatComplex(p)).join("；")],
      ["极限方向", nyquistLimitText(data)],
    ];
    $("#nyquist-summary").innerHTML = rows.map(([label, value]) => "<span>" + label + "</span><b>" + value + "</b>").join("");
    $("#stability-note").textContent = "绿色：正频率；蓝色：负频率；红点：(−1,0)；灰虚线：渐近线；紫虚线：绕行映射。完整围线按 ω 从 −∞ 到 +∞，逆时针 N 为正，Z=P−N。" +
      (data.imaginaryAxisPole ? "虚轴极点从右侧小半圆绕开；映射按精确坐标绘制，可能在视野外。" : "") +
      "模值 >10000 使用虚线，Auto 不包含全部远端。";
    setParameterCards([
      { label: "劳斯判据", value: status, accent: unstable || axis.length > 0 }, { label: "首列变号", value: table.signChanges, unit: "次" },
      { label: "Nyquist 判据", value: nyquistStatus }, { label: "逆时针环绕数", value: reliable ? winding : null },
      { label: "环路增益 K", value: gain }, { label: "远端模值阈值", value: FAR_RADIUS },
    ], "稳定性 · 精确 Nyquist 坐标图");
  }


  function calculate() {
    ui.error.hidden = true;
    $("#cursor-readout").textContent = plotHint;
    syncPlotControls();
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
  ["magnitude-scale-mode", "phase-scale-mode", "nyquist-scale-mode", "magnitude-min", "magnitude-max", "magnitude-step", "phase-min", "phase-max", "phase-step", "nyquist-x-min", "nyquist-x-max", "nyquist-x-step", "nyquist-y-min", "nyquist-y-max", "nyquist-y-step", "nyquist-negative", "nyquist-detours", "frequency-min", "frequency-max"].forEach((id) => {
    $("#" + id).addEventListener("change", () => { clearPlotViews(); calculate(); });
  });
  document.querySelectorAll("[data-auto]").forEach((button) => button.addEventListener("click", () => {
    $("#" + button.dataset.auto + "-scale-mode").value = "auto"; clearPlotViews(); calculate();
  }));
  $("#root-auto").addEventListener("click", () => { clearPlotViews(); calculate(); });
  [ui.numerator, ui.denominator, ui.formulaInput, ui.responseTime, ui.inputSignal, ui.outputResponse, ui.duration].forEach((input) => input.addEventListener("keydown", (event) => { if (event.key === "Enter" && !event.shiftKey) calculate(); }));
  ui.bodeQueryMode.addEventListener("change", () => {
    const phaseMode = ui.bodeQueryMode.value === "phase-at-frequency";
    ui.bodeQueryPrefix.textContent = phaseMode ? "ω" : "φ";
    ui.bodeQueryUnit.textContent = phaseMode ? "rad/s" : "deg";
    if (lastModel && lastModel.mode === "model" && currentView === "frequency") calculate();
  });
  ui.bodeQueryValue.addEventListener("input", () => { if (lastModel && lastModel.mode === "model" && currentView === "frequency") calculate(); });
  if (typeof ResizeObserver !== "undefined") {
    let resizeFrame;
    resizeObserver = new ResizeObserver(() => {
      cancelAnimationFrame(resizeFrame);
      resizeFrame = requestAnimationFrame(() => activeCharts.forEach((chart) => {
        const state = lineChartState.get(chart);
        if (chart.getClientRects().length && state?.baseOptions) makeSvgLineChart(chart, state.baseSeries, { ...state.baseOptions, interaction: true });
      }));
    });
    [ui.stepChart, ui.bodeChart, ui.phaseChart, ui.nyquistChart, ui.rootChart, ui.stabilityNyquistChart].forEach((chart) => resizeObserver.observe(chart));
  } else window.addEventListener("resize", () => { if (lastModel) calculate(); });
  calculate();
}

const ControlLabMath = { parsePolynomial, parseTransferFunction, parseNumberList, polynomialRoots, validateModel, simulateStep, calculateMetrics, frequencyResponse, formatPolynomial, modelCharacteristics, findBreakawayPoints, findImaginaryCrossings, estimateCrossover, estimateCrossovers, interpolateFrequencyField, evaluateQuery, routhTable, closedLoopPolynomial, nyquistData, nyquistAutoBounds, nyquistWinding, frequencySamples, rootLocusData, axisTicks, niceStep, niceBounds, radiusSegments, clipSegment, FAR_RADIUS };
if (typeof module !== "undefined" && module.exports) module.exports = ControlLabMath;
if (typeof document !== "undefined") initApp();
