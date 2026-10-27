'use strict';
/* =====================================================================
   MOTOR NUMÉRICO  (independiente del DOM)
   I ≈ h·k·Σ cᵢ·fᵢ   con nodos xᵢ = a + i·h,  h = (b−a)/n
   ===================================================================== */
const MATH = {sin:Math.sin,cos:Math.cos,tan:Math.tan,asin:Math.asin,acos:Math.acos,atan:Math.atan,
  sinh:Math.sinh,cosh:Math.cosh,tanh:Math.tanh,sqrt:Math.sqrt,exp:Math.exp,ln:Math.log,log:Math.log,
  log10:Math.log10,abs:Math.abs,pow:Math.pow,min:Math.min,max:Math.max,pi:Math.PI,e:Math.E};

const METHODS = {
  trap:{name:'Trapecio compuesto', pre:'h/2'},
  s13: {name:'Simpson 1/3',        pre:'h/3'},
  s38: {name:'Simpson 3/8',        pre:'3h/8'},
  comb:{name:'Combinado 1/3 + 3/8',pre:'h/24'}
};

/* Compila una expresión de texto de forma segura (lista blanca de nombres). */
function compileExpr(src, vars = []) {
  const s = String(src ?? '').trim().toLowerCase().replace(/π/g, 'pi').replace(/\^/g, '**');
  if (!s) throw new Error('La expresión está vacía.');
  const stripped = s.replace(/(\d+\.?\d*|\.\d+)(e[+-]?\d+)?/g, ' ');
  for (const id of stripped.match(/[a-z_][a-z0-9_]*/g) || [])
    if (!vars.includes(id) && !(id in MATH)) throw new Error(`Nombre desconocido en la expresión: «${id}».`);
  const rest = stripped.replace(/[a-z_][a-z0-9_]*/g, ' ');
  const bad = rest.match(/[^\s+\-*/().,]/);
  if (bad) throw new Error(`Carácter no permitido en la expresión: «${bad[0]}».`);
  let fn;
  try { fn = new Function(...vars, ...Object.keys(MATH), `"use strict";return (${s});`); }
  catch (_) { throw new Error(`Expresión inválida «${src}». Use * explícito (2*x) y paréntesis, p. ej. -(x^2).`); }
  const vals = Object.values(MATH);
  return (...a) => fn(...a, ...vals);
}
function evalConst(src, label) {
  const v = compileExpr(src, [])();
  if (!Number.isFinite(v)) throw new Error(`El ${label} no es un número real.`);
  return v;
}

/* Lista de números separados por comas, espacios, ; o saltos de línea. */
function parseList(str, label) {
  const toks = String(str).split(/[\s,;]+/).filter(Boolean);
  return toks.map(t => {
    const v = Number(t);
    if (!Number.isFinite(v)) throw new Error(`«${t}» no es un número válido (lista ${label}).`);
    return v;
  });
}

/* Verifica x estrictamente creciente y espaciamiento uniforme; devuelve h. */
function checkGrid(xs) {
  if (xs.length < 2) throw new Error('Se necesitan al menos 2 puntos (n ≥ 1).');
  for (let i = 1; i < xs.length; i++)
    if (!(xs[i] > xs[i - 1])) throw new Error(`x debe ser estrictamente creciente: falla en la posición ${i + 1} (${xs[i - 1]} → ${xs[i]}).`);
  const n = xs.length - 1, h = (xs[n] - xs[0]) / n;
  for (let i = 1; i <= n; i++)
    if (Math.abs((xs[i] - xs[i - 1]) - h) > 1e-6 * Math.abs(h))
      throw new Error(`El espaciamiento no es uniforme: Δx entre los puntos ${i} y ${i + 1} es ${+(xs[i] - xs[i - 1]).toPrecision(8)}, pero h = ${+h.toPrecision(8)}. Estos métodos exigen nodos igualmente espaciados.`);
  return h;
}

/* Muestrea f en n+1 nodos igualmente espaciados. */
function sample(f, a, b, n) {
  const h = (b - a) / n, xs = [], fs = [];
  for (let i = 0; i <= n; i++) {
    const x = i === n ? b : a + i * h, y = f(x);
    if (!Number.isFinite(y)) throw new Error(`f(x) no es un número real en x = ${+x.toPrecision(8)}.`);
    xs.push(x); fs.push(y);
  }
  return {xs, fs, h, n};
}

/* Coeficientes cᵢ y prefactor k de cada método, o la razón por la que no aplica. */
function coefs(method, n) {
  const c = new Array(n + 1).fill(0);
  if (method === 'trap') {
    c.fill(2); c[0] = c[n] = 1; return {c, k: 1 / 2};
  }
  if (method === 's13') {
    if (n < 2 || n % 2) return {why: `n = ${n} no es par (Simpson 1/3 requiere n par).`};
    for (let i = 0; i <= n; i++) c[i] = (i === 0 || i === n) ? 1 : (i % 2 ? 4 : 2);
    return {c, k: 1 / 3};
  }
  if (method === 's38') {
    if (n < 3 || n % 3) return {why: `n = ${n} no es múltiplo de 3 (Simpson 3/8 lo requiere).`};
    for (let i = 0; i <= n; i++) c[i] = (i === 0 || i === n) ? 1 : (i % 3 ? 3 : 2);
    return {c, k: 3 / 8};
  }
  /* combinado: 1/3 en los primeros n−3 subintervalos y 3/8 en los últimos 3 (n impar ≥ 5) */
  if (n < 5 || n % 2 === 0) return {why: `El combinado requiere n impar ≥ 5 (aquí n = ${n}).`};
  const m = n - 3;
  for (let i = 0; i <= m; i++) c[i] += 8 * ((i === 0 || i === m) ? 1 : (i % 2 ? 4 : 2));
  for (let j = 0; j <= 3; j++) c[m + j] += 9 * ((j === 0 || j === 3) ? 1 : 3);
  return {c, k: 1 / 24};
}

function integrate(method, fs, h) {
  const n = fs.length - 1, cf = coefs(method, n);
  if (cf.why) return {ok: false, why: cf.why};
  let sum = 0;
  for (let i = 0; i <= n; i++) sum += cf.c[i] * fs[i];
  return {ok: true, value: h * cf.k * sum, sum, pref: h * cf.k, c: cf.c};
}
const integrateAll = (fs, h) => Object.fromEntries(Object.keys(METHODS).map(m => [m, integrate(m, fs, h)]));

/* ===== UI ===== */
/* =====================================================================
   EJERCICIOS (enunciados y datos del documento del desafío)
   ===================================================================== */
const PRESETS = [
 {id:'19',grp:'AD',mode:'data',title:'Ejercicio 19 · Transporte urbano',
  ctx:'Un vehículo experimental registra su velocidad durante los primeros 7 s. Se estima la distancia recorrida integrando v(t).',
  model:'d = ∫₀⁷ v(t) dt  →  d [mi] = (1/3600)·∫ v dt   (v en mi/h, t en s)',
  x:'0,1,2,3,4,5,6,7',y:'0,14,39,69,95,114,129,139',xu:'s',yu:'mi/h',ru:'mi',factor:'1/3600',alt:{f:1609.344,u:'m'}},
 {id:'20',grp:'BE',mode:'data',title:'Ejercicio 20 · Ensayo de elasticidad',
  ctx:'Una banda elástica se estira y se mide la fuerza a distintos desplazamientos. El trabajo es el área bajo F(x).',
  model:'W = ∫₀²·⁸ F(x) dx   (F en lb, x en in)',
  x:'0,0.4,0.8,1.2,1.6,2.0,2.4,2.8',y:'0,0.85,1.30,1.60,1.87,2.14,2.34,2.52',xu:'in',yu:'lb',ru:'lb·in',alt:{f:0.1129848,u:'J'}},
 {id:'21',grp:'C',mode:'data',title:'Ejercicio 21 · Perfil topográfico',
  ctx:'Área bajo un perfil de altura digitalizado entre x = 1 y x = 10 (9 subintervalos).',
  model:'A = ∫₁¹⁰ y(x) dx',warn:'Datos ilustrativos extraídos visualmente de la figura (not exactos). La precisión depende de la digitalización.',
  x:'1,2,3,4,5,6,7,8,9,10',y:'1.3,3.7,4.6,4.4,2.7,2.5,3.6,5.3,6.0,5.9',xu:'u',yu:'u',ru:'u²'},
 {id:'22',grp:'D',mode:'func',title:'Ejercicio 22 · Cable suspendido (catenaria)',
  ctx:'Longitud de un cable con forma y = a·cosh(x/a), a = 24 ft, entre x = −12 ft y x = 26 ft. Modelo geométrico simplificado, no un cálculo de ingeniería del teleférico.',
  model:'L = ∫₋₁₂²⁶ √(1 + [y′]²) dx,   y′ = sinh(x/24)   (exacto: 24[sinh(26/24) − sinh(−12/24)])',
  fx:'sqrt(1+sinh(x/24)^2)',a:'-12',b:'26',n:'8',ref:'24*(sinh(26/24)-sinh(-12/24))',xu:'ft',yu:'',ru:'ft'},
 {id:'23',grp:'AF',mode:'func',title:'Ejercicio 23 · Red de abastecimiento de agua',
  ctx:'Caudal de una tubería circular con perfil turbulento v = vmax(1 − r/R)^(1/n), R = 0,25 in, n = 7, vmax = 80 in/s. La derivada cerca de la pared frena la convergencia.',
  model:'Q = ∫₀ᴿ 2π·r·v(r) dr,  v(r) = 80(1 − r/0.25)^(1/7)   (exacto: 49π/12)',
  fx:'2*pi*x*80*(1-x/0.25)^(1/7)',a:'0',b:'0.25',n:'8',ref:'49*pi/12',xu:'in',yu:'in²/s',ru:'in³/s'},
 {id:'24',grp:'C',mode:'func',title:'Ejercicio 24 · Trayectoria (cardioide)',
  ctx:'Longitud de la cardioide x = 2b·cos t − b·cos 2t,  y = 2b·sin t − b·sin 2t, con b = 5 y 0 ≤ t ≤ 2π.',
  model:'L = ∫₀²ᵖⁱ √([x′]² + [y′]²) dt = ∫ 20·sin(t/2) dt = 80',cardio:true,
  fx:'sqrt((-10*sin(x)+10*sin(2*x))^2+(10*cos(x)-10*cos(2*x))^2)',a:'0',b:'2*pi',n:'8',ref:'80',xu:'rad',yu:'',ru:'u'},
 {id:'25',grp:'B',mode:'func',title:'Ejercicio 25 · Energía gravitatoria',
  ctx:'Variación de energía potencial de un satélite de 500 kg elevado de la superficie a 800 km, con g(y) = g₀R²/(R + y)².',
  model:'ΔU = ∫₀ʰ m·g(y) dy = m·g₀·R·h/(R + h)   (R = 6 371 000 m, g₀ = 9,81 m/s²)',
  fx:'500*9.81*6371000^2/(6371000+x)^2',a:'0',b:'800000',n:'8',ref:'500*9.81*6371000*800000/(6371000+800000)',xu:'m',yu:'N',ru:'J'},
 {id:'26',grp:'F',mode:'data',title:'Ejercicio 26 · Superficie de un lago',
  ctx:'Área del lago Erie a partir de anchuras medidas cada 20 mi, de x = 40 a x = 260 (n = 11). Referencia: 9 940 mi². Luego se propone adaptar el método al lago Titicaca con datos geográficos oficiales.',
  model:'A = ∫₄₀²⁶⁰ w(x) dx   (x y w en millas)',
  warn:'Anchuras ILUSTRATIVAS: reemplácelas por la tabla común digitalizada del mapa que entregue el docente.',
  x:'40,60,80,100,120,140,160,180,200,220,240,260',y:'20,38,52,58,52,58,62,56,52,46,34,20',xu:'mi',yu:'mi',ru:'mi²',ref:'9940'},
 {id:'27a',grp:'E',mode:'data',title:'Ejercicio 27 · Volumen del balón',
  ctx:'Diámetro d de un balón medido a lo largo del eje z. Con r = d/2, el volumen es V = π∫r² dz.',
  model:'V = π∫₀¹² (d/2)² dz   (z y d en in)',
  x:'0,1,2,3,4,5,6,7,8,9,10,11,12',y:'0,2.6,3.2,4.8,5.6,6.0,6.2,6.0,5.6,4.8,3.3,2.6,0',g:'pi*(y/2)^2',xu:'in',yu:'in',ru:'in³'},
 {id:'27b',grp:'E',mode:'data',title:'Ejercicio 27 · Superficie (fórmula del enunciado)',
  ctx:'Se aplica S = 2π∫r dz tal como aparece en el enunciado.',
  model:'S = 2π∫₀¹² (d/2) dz   (z y d en in)',
  warn:'Esta fórmula omite la pendiente: no es el área real de revolución. El área exacta exige S = 2π∫ r·√(1+(r′)²) dz y una estimación de r′.',
  x:'0,1,2,3,4,5,6,7,8,9,10,11,12',y:'0,2.6,3.2,4.8,5.6,6.0,6.2,6.0,5.6,4.8,3.3,2.6,0',g:'2*pi*(y/2)',xu:'in',yu:'in',ru:'in²'}
];

const $ = id => document.getElementById(id);
const esc = s => String(s).replace(/[&<>"]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[c]));
let mode = 'data', last = null, activeId = null;

function fmt(v) { return Number.isFinite(v) ? v.toFixed(6) : '—'; }
function fmtSci(v) { return Math.abs(v) >= 1e6 ? v.toExponential(8) : ''; }

/* ---------- mensajes ---------- */
function showMsg(text, kind) {
  const m = $('msg'); m.className = kind; m.textContent = text; m.hidden = !text;
}

/* ---------- ejercicios ---------- */
function buildChips() {
  $('chips').innerHTML = PRESETS.map(p => `<button type="button" class="chip" data-id="${p.id}" data-grp="${p.grp}">
    <b>${esc(p.title.split(' · ')[0])}</b><span>${esc(p.title.split(' · ')[1])}</span></button>`).join('');
  $('chips').addEventListener('click', e => {
    const b = e.target.closest('.chip'); if (b) loadPreset(b.dataset.id);
  });
}
function filterGroup() {
  const g = $('grupo').value;
  document.querySelectorAll('.chip').forEach(c => c.classList.toggle('dim', g && !c.dataset.grp.includes(g)));
}
function setMode(m) {
  mode = m;
  $('tabData').classList.toggle('on', m === 'data'); $('tabFunc').classList.toggle('on', m === 'func');
  $('panelData').hidden = m !== 'data'; $('panelFunc').hidden = m !== 'func';
}
function loadPreset(id) {
  const p = PRESETS.find(q => q.id === id); if (!p) return;
  activeId = id; setMode(p.mode);
  document.querySelectorAll('.chip').forEach(c => c.classList.toggle('on', c.dataset.id === id));
  const set = (k, v) => { $(k).value = v ?? ''; };
  set('xs', (p.x || '').replace(/,/g, ', ')); set('ys', (p.y || '').replace(/,/g, ', ')); set('g', p.g); set('factor', p.factor);
  set('fx', p.fx); set('a', p.a); set('b', p.b); set('n', p.n);
  set('xu', p.xu); set('yu', p.yu); set('ru', p.ru); set('ref', p.ref); $('metodo').value = 'all';
  $('plan').hidden = false; $('planTitle').textContent = p.title; $('planCtx').textContent = p.ctx;
  $('planModel').textContent = p.model; $('planWarn').hidden = !p.warn; $('planWarn').textContent = '⚠ ' + (p.warn || '');
  $('cardioWrap').hidden = !p.cardio; if (p.cardio) drawCardioid();
  calculate();
}
function clearAll() {
  ['xs','ys','g','factor','fx','a','b','n','xu','yu','ru','ref'].forEach(k => $(k).value = '');
  $('metodo').value = 'all'; activeId = null; last = null;
  document.querySelectorAll('.chip').forEach(c => c.classList.remove('on'));
  $('plan').hidden = true; $('out').hidden = true; $('testCard').hidden = true; showMsg('', '');
}

/* ---------- lectura y cálculo ---------- */
function readInputs() {
  const units = {xu: $('xu').value.trim(), yu: $('yu').value.trim(), ru: $('ru').value.trim()};
  const refTxt = $('ref').value.trim();
  const ref = refTxt ? evalConst(refTxt, 'valor de referencia') : null;
  const p = PRESETS.find(q => q.id === activeId);
  let calcN, n, factor = 1, curve = null, ylabel = units.yu;
  if (mode === 'data') {
    const xs = parseList($('xs').value, 'X'), ys = parseList($('ys').value, 'Y');
    if (!xs.length || !ys.length) throw new Error('Ingrese los valores de x e y.');
    if (xs.length !== ys.length) throw new Error(`Las listas tienen longitud distinta: X tiene ${xs.length} valores y Y tiene ${ys.length}.`);
    const h = checkGrid(xs);
    const gT = $('g').value.trim(), g = gT ? compileExpr(gT, ['y']) : (y => y);
    const fs = ys.map((y, i) => {
      const v = g(y); if (!Number.isFinite(v)) throw new Error(`g(y) no es real para y = ${y} (posición ${i + 1}).`); return v;
    });
    const fT = $('factor').value.trim(); if (fT) factor = evalConst(fT, 'factor de conversión');
    if (gT) ylabel = 'g(y)';
    n = xs.length - 1; calcN = () => ({xs, fs, h, n});
  } else {
    const f = compileExpr($('fx').value, ['x']);
    if (!$('a').value.trim() || !$('b').value.trim()) throw new Error('Ingrese los límites a y b.');
    const a = evalConst($('a').value, 'límite a'), b = evalConst($('b').value, 'límite b');
    const nStr = $('n').value.trim(); n = Number(nStr);
    if (!nStr || !Number.isInteger(n) || n < 1) throw new Error('n debe ser un entero positivo (número de subintervalos).');
    if (n > 200000) throw new Error('n es demasiado grande (máximo 200 000).');
    if (!(a < b)) throw new Error(`Se requiere a < b (recibido a = ${a}, b = ${b}).`);
    calcN = m => sample(f, a, b, m);
    curve = []; for (let i = 0; i <= 300; i++) { const x = a + (b - a) * i / 300, y = f(x); if (Number.isFinite(y)) curve.push([x, y]); }
    ylabel = 'f(x)' + (units.yu ? ' [' + units.yu + ']' : '');
  }
  return {units, ref, factor, calcN, n, curve, ylabel, alt: p && p.alt, fnMode: mode === 'func'};
}

function calculate() {
  try {
    showMsg('', '');
    const inp = readInputs(), sel = $('metodo').value;
    const base = inp.calcN(inp.n), res = integrateAll(base.fs, base.h);
    if (sel !== 'all' && !res[sel].ok) throw new Error(`${METHODS[sel].name}: ${res[sel].why} Elija otro método o cambie n.`);
    last = {inp, base, res, sel};
    render();
  } catch (err) {
    $('out').hidden = true; showMsg('⚠ ' + err.message, 'err');
  }
}

/* ---------- render ---------- */
function errTxt(v, ref) {
  if (ref === null) return '';
  const ea = Math.abs(ref - v), ep = ref !== 0 ? 100 * ea / Math.abs(ref) : null;
  return `Eₐ = ${ea.toFixed(6)} · E% = ${ep === null ? '—' : ep.toFixed(6) + ' %'}`;
}
function render() {
  const {inp, base, res, sel} = last, {units, ref, factor, alt} = inp, u = units.ru;
  $('out').hidden = false;
  $('summary').innerHTML = [
    ['n', base.n], ['h', +base.h.toPrecision(10) + (units.xu ? ' ' + units.xu : '')],
    ['Intervalo', `[${+base.xs[0].toPrecision(8)}, ${+base.xs[base.n].toPrecision(8)}]`],
    ['Nodos', base.n + 1], ref !== null ? ['Referencia', fmt(ref) + (u ? ' ' + u : '')] : null,
    factor !== 1 ? ['Factor', +factor.toPrecision(10)] : null
  ].filter(Boolean).map(([k, v]) => `<span class="pill">${esc(k)} = <b>${esc(v)}</b></span>`).join('');

  const keys = sel === 'all' ? Object.keys(METHODS) : [sel];
  $('cards').innerHTML = keys.map(k => {
    const r = res[k];
    if (!r.ok) return `<article class="res na"><h3>${METHODS[k].name}</h3><div class="val">No aplicable</div><p>${esc(r.why)}</p></article>`;
    const v = r.value * factor, sci = fmtSci(v);
    return `<article class="res"><h3>${METHODS[k].name}</h3>
      <div class="val">${fmt(v)} <small>${esc(u)}</small></div>
      ${sci ? `<p>≈ ${sci}</p>` : ''}${alt ? `<p>≈ ${(v * alt.f).toFixed(6)} ${esc(alt.u)}</p>` : ''}
      ${ref !== null ? `<p>${errTxt(v, ref)}</p>` : '<p>Sin valor de referencia.</p>'}</article>`;
  }).join('');

  renderCompare(); renderWeights(); drawChart();
}

function renderCompare() {
  const {inp, base} = last, {ref, factor, units} = inp;
  const ns = inp.fnMode ? [...new Set([4, 8, base.n])].sort((p, q) => p - q) : [base.n];
  const rows = [];
  ns.forEach((m, idx) => {
    const d = m === base.n ? base : inp.calcN(m), r = integrateAll(d.fs, d.h);
    Object.keys(METHODS).forEach((k, j) => {
      const x = r[k];
      const v = x.ok ? x.value * factor : NaN, ea = x.ok && ref !== null ? Math.abs(ref - v) : NaN;
      rows.push(`<tr class="${j === 0 && idx ? 'sep' : ''}"><td>${j === 0 ? m + (m === base.n ? ' ★' : '') : ''}</td><td>${j === 0 ? +d.h.toPrecision(8) : ''}</td>
        <td class="l">${METHODS[k].name}</td><td>${x.ok ? fmt(v) : '<span title="' + esc(x.why) + '">n/a</span>'}</td>
        <td>${Number.isFinite(ea) ? ea.toFixed(6) : '—'}</td>
        <td>${Number.isFinite(ea) && ref !== 0 ? (100 * ea / Math.abs(ref)).toFixed(6) : '—'}</td></tr>`);
    });
  });
  $('cmp').innerHTML = `<table><thead><tr><th>n</th><th>h</th><th class="l">Método</th><th>Valor [${esc(units.ru)}]</th><th>Eₐ</th><th>E %</th></tr></thead><tbody>${rows.join('')}</tbody></table>
    <p class="hint">★ = n elegido. «n/a» = el método no es aplicable con ese n. ${ref === null ? 'Sin valor de referencia no hay error verdadero.' : ''}</p>`;
}

function renderWeights() {
  const {base, res, inp} = last, {xs, fs, n} = base, LIM = 60, keys = Object.keys(METHODS);
  const show = Math.min(n, LIM);
  const head = keys.map(k => `<th title="${esc(res[k].ok ? '' : res[k].why)}">${METHODS[k].name} (${METHODS[k].pre})</th>`).join('');
  let body = '';
  for (let i = 0; i <= show; i++)
    body += `<tr><td>${i}</td><td>${+xs[i].toPrecision(8)}</td><td>${+fs[i].toPrecision(8)}</td>${keys.map(k => `<td>${res[k].ok ? res[k].c[i] : '—'}</td>`).join('')}</tr>`;
  if (n > LIM) body += `<tr><td colspan="7" class="l">… ${n - LIM} filas más no mostradas.</td></tr>`;
  const foot = `<tr><td colspan="3" class="l">Σ cᵢ·fᵢ</td>${keys.map(k => `<td>${res[k].ok ? res[k].sum.toPrecision(10) : '—'}</td>`).join('')}</tr>
    <tr><td colspan="3" class="l">Prefactor</td>${keys.map(k => `<td>${res[k].ok ? +res[k].pref.toPrecision(10) : '—'}</td>`).join('')}</tr>
    <tr><td colspan="3" class="l">Integral final</td>${keys.map(k => `<td>${res[k].ok ? fmt(res[k].value * inp.factor) : '—'}</td>`).join('')}</tr>`;
  $('wts').innerHTML = `<table><thead><tr><th>i</th><th>xᵢ</th><th>f(xᵢ)</th>${head}</tr></thead><tbody>${body}</tbody><tfoot>${foot}</tfoot></table>
    <p class="hint">I ≈ prefactor · Σ cᵢ·fᵢ</p>`;
}

/* ---------- gráficos ---------- */
const css = v => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
function prep(canvas) {
  const dpr = window.devicePixelRatio || 1, W = canvas.clientWidth, H = canvas.clientHeight;
  canvas.width = W * dpr; canvas.height = H * dpr;
  const c = canvas.getContext('2d'); c.setTransform(dpr, 0, 0, dpr, 0, 0); c.clearRect(0, 0, W, H);
  return {c, W, H};
}
const tick = v => (Math.abs(v) >= 1e5 || (v !== 0 && Math.abs(v) < 1e-2)) ? v.toExponential(1) : String(+v.toPrecision(3));

function drawChart() {
  if (!last) return;
  const {base, inp} = last, {xs, fs, n} = base, {c, W, H} = prep($('chart'));
  const pts = inp.curve || xs.map((x, i) => [x, fs[i]]);
  const M = {l: 58, r: 16, t: 14, b: 38};
  const all = pts.map(p => p[1]).concat(fs);
  let y0 = Math.min(0, ...all), y1 = Math.max(0, ...all); if (y1 === y0) y1 = y0 + 1; y1 += (y1 - y0) * 0.08;
  const x0 = xs[0], x1 = xs[n];
  const sx = x => M.l + (x - x0) / (x1 - x0) * (W - M.l - M.r), sy = y => H - M.b - (y - y0) / (y1 - y0) * (H - M.t - M.b);
  const ink = css('--ink'), mut = css('--muted'), line = css('--line'), acc = css('--accent'), teal = css('--teal');
  c.font = '11px system-ui'; c.fillStyle = mut; c.strokeStyle = line; c.lineWidth = 1;
  for (let i = 0; i <= 5; i++) {
    const yv = y0 + (y1 - y0) * i / 5, xv = x0 + (x1 - x0) * i / 5;
    c.beginPath(); c.moveTo(M.l, sy(yv)); c.lineTo(W - M.r, sy(yv)); c.stroke();
    c.textAlign = 'right'; c.fillText(tick(yv), M.l - 6, sy(yv) + 4);
    c.textAlign = 'center'; c.fillText(tick(xv), sx(xv), H - M.b + 16);
  }
  c.fillStyle = acc; c.globalAlpha = 0.12; c.strokeStyle = acc;
  for (let i = 0; i < n; i++) {
    c.beginPath(); c.moveTo(sx(xs[i]), sy(0)); c.lineTo(sx(xs[i]), sy(fs[i])); c.lineTo(sx(xs[i + 1]), sy(fs[i + 1])); c.lineTo(sx(xs[i + 1]), sy(0)); c.closePath(); c.fill();
    c.globalAlpha = 0.4; c.lineWidth = 1; c.stroke(); c.globalAlpha = 0.12;
  }
  c.globalAlpha = 1; c.strokeStyle = ink; c.lineWidth = 1.2; c.beginPath(); c.moveTo(M.l, sy(0)); c.lineTo(W - M.r, sy(0)); c.stroke();
  c.strokeStyle = teal; c.lineWidth = 2.4; c.beginPath();
  pts.forEach((p, i) => i ? c.lineTo(sx(p[0]), sy(p[1])) : c.moveTo(sx(p[0]), sy(p[1]))); c.stroke();
  if (n <= 60) { c.fillStyle = acc; xs.forEach((x, i) => { c.beginPath(); c.arc(sx(x), sy(fs[i]), 3.5, 0, 7); c.fill(); }); }
  c.fillStyle = mut; c.textAlign = 'center'; c.fillText(`x${inp.units.xu ? ' [' + inp.units.xu + ']' : ''}`, (M.l + W - M.r) / 2, H - 6);
  c.save(); c.translate(12, (M.t + H - M.b) / 2); c.rotate(-Math.PI / 2); c.fillText(inp.ylabel, 0, 0); c.restore();
}

function drawCardioid() {
  const {c, W, H} = prep($('cardio')), b = 5, k = Math.min(W, H) / 40;
  const P = t => [W / 2 + (2 * b * Math.cos(t) - b * Math.cos(2 * t) - 5) * k * 1.0, H / 2 - (2 * b * Math.sin(t) - b * Math.sin(2 * t)) * k];
  c.strokeStyle = css('--line'); c.lineWidth = 1;
  c.beginPath(); c.moveTo(8, H / 2); c.lineTo(W - 8, H / 2); c.moveTo(W / 2 - 5 * k, 8); c.lineTo(W / 2 - 5 * k, H - 8); c.stroke();
  c.strokeStyle = css('--accent'); c.lineWidth = 2.6; c.beginPath();
  for (let i = 0; i <= 400; i++) { const p = P(2 * Math.PI * i / 400); i ? c.lineTo(...p) : c.moveTo(...p); }
  c.stroke();
}

/* ---------- prueba de validación ---------- */
function runTest() {
  const f = compileExpr('x^2', ['x']), exact = 8 / 3, rows = [];
  let allOk = true;
  [4, 6, 8].forEach(n => {
    const d = sample(f, 0, 2, n), r = integrateAll(d.fs, d.h);
    ['trap', 's13', 's38'].forEach(k => {
      if (!r[k].ok) { rows.push(`<tr><td>${n}</td><td class="l">${METHODS[k].name}</td><td>n/a</td><td>—</td><td class="l">${esc(r[k].why)}</td></tr>`); return; }
      const ea = Math.abs(exact - r[k].value), exactMethod = k !== 'trap', ok = exactMethod ? ea < 1e-12 : ea < 0.1;
      allOk = allOk && ok;
      rows.push(`<tr><td>${n}</td><td class="l">${METHODS[k].name}</td><td>${fmt(r[k].value)}</td><td>${ea.toFixed(6)}</td>
        <td class="l ${ok ? 'ok' : 'ko'}">${ok ? '✔ ' + (exactMethod ? 'exacto' : 'ok') : '✘'}</td></tr>`);
    });
  });
  const t4 = Math.abs(exact - integrate('trap', sample(f, 0, 2, 4).fs, 0.5).value), t8 = Math.abs(exact - integrate('trap', sample(f, 0, 2, 8).fs, 0.25).value);
  const ratio = t4 / t8, rOk = Math.abs(ratio - 4) < 0.01; allOk = allOk && rOk;
  $('testOut').innerHTML = `<p>Valor exacto: <b>${fmt(exact)}</b> (8/3). Resultado global: <b class="${allOk ? 'ok' : 'ko'}">${allOk ? '✔ APROBADO' : '✘ REVISAR'}</b></p>
    <table><thead><tr><th>n</th><th class="l">Método</th><th>Valor</th><th>Eₐ</th><th class="l">Veredicto</th></tr></thead><tbody>${rows.join('')}</tbody></table>
    <p class="hint">Al duplicar n (4 → 8) el error del trapecio baja ${ratio.toFixed(3)} veces (se esperan ≈ 4) <b class="${rOk ? 'ok' : 'ko'}">${rOk ? '✔' : '✘'}</b>.</p>`;
  $('testCard').hidden = false; $('testCard').scrollIntoView({behavior: 'smooth', block: 'nearest'});
}

/* ---------- eventos ---------- */
buildChips(); setMode('data');
$('grupo').addEventListener('change', filterGroup);
$('tabData').addEventListener('click', () => setMode('data'));
$('tabFunc').addEventListener('click', () => setMode('func'));
$('calc').addEventListener('click', calculate);
$('test').addEventListener('click', () => { try { runTest(); } catch (e) { showMsg('⚠ ' + e.message, 'err'); } });
$('clear').addEventListener('click', clearAll);
$('print').addEventListener('click', () => window.print());
$('metodo').addEventListener('change', () => { if (last) calculate(); });
window.addEventListener('resize', () => { if (last) drawChart(); if (!$('cardioWrap').hidden) drawCardioid(); });
document.addEventListener('keydown', e => { if (e.key === 'Enter' && e.target.tagName === 'INPUT') calculate(); });