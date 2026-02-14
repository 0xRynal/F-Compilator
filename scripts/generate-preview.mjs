#!/usr/bin/env node
/**
 * Génère preview.png à partir de docs/preview.html (Puppeteer).
 * À lancer depuis la racine du repo :  node scripts/generate-preview.mjs
 * Ou :  cd scripts && npm install && npm run preview
 */

import puppeteer from 'puppeteer';
import { fileURLToPath, pathToFileURL } from 'url';
import path from 'path';
import fs from 'fs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, '..');
const htmlPath = path.join(repoRoot, 'docs', 'preview.html');
const outPath = path.join(repoRoot, 'preview.png');

if (!fs.existsSync(htmlPath)) {
  console.error('Fichier introuvable:', htmlPath);
  process.exit(1);
}

const fileUrl = pathToFileURL(htmlPath).href;

const browser = await puppeteer.launch({ headless: true });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 800, height: 520, deviceScaleFactor: 2 });
  await page.goto(fileUrl, {
    waitUntil: 'load',
  });
  await page.evaluate(() => document.body.offsetHeight);
  const frame = await page.$('.frame');
  if (frame) {
    await frame.screenshot({ path: outPath });
  } else {
    await page.screenshot({ path: outPath });
  }
  console.log('Preview générée:', outPath);
} finally {
  await browser.close();
}
