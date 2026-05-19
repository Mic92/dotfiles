"use strict";

/** @typedef {{ name: string, url: string, scope: string, color?: string, icon?: string }} App */

// Valid Firefox container colors: blue, turquoise, green, yellow, orange, red, pink, purple
// Valid icons: fingerprint, briefcase, dollar, cart, circle, gift, vacation, food, fruit, pet, tree, chill, fence

/** @type {App[]} */
const DEFAULT_APPS = [
  {
    name: "WhatsApp",
    url: "https://web.whatsapp.com/",
    scope: "web.whatsapp.com",
    color: "green",
    icon: "circle",
  },
  {
    name: "Discord",
    url: "https://discord.com/app",
    scope: "discord.com",
    color: "purple",
    icon: "circle",
  },
  {
    name: "Element",
    url: "https://matrix.thalheim.io/",
    scope: "matrix.thalheim.io",
    color: "turquoise",
    icon: "circle",
  },
  {
    name: "Slack Numtide",
    url: "https://numtide.slack.com/",
    scope: "slack.com",
    color: "blue",
    icon: "briefcase",
  },
  {
    name: "Slack TUM",
    url: "https://ls1-tum.slack.com/",
    scope: "slack.com",
    color: "orange",
    icon: "briefcase",
  },
  {
    name: "Slack TII",
    url: "https://securesystem-71s3304.slack.com/",
    scope: "slack.com",
    color: "red",
    icon: "briefcase",
  },
];
