import js from "@eslint/js";
import jsdoc from "eslint-plugin-jsdoc";
import unicorn from "eslint-plugin-unicorn";

export default [
  js.configs.recommended,
  unicorn.configs.recommended,
  {
    files: ["*.js"],
    languageOptions: {
      ecmaVersion: 2023,
      sourceType: "module",
      globals: {
        // Browser extension globals
        browser: "readonly",
        chrome: "readonly",

        // Web API globals
        console: "readonly",
        window: "readonly",
        document: "readonly",
        XMLHttpRequest: "readonly",
        WebSocket: "readonly",
        Element: "readonly",
        HTMLElement: "readonly",
        HTMLInputElement: "readonly",
        HTMLTextAreaElement: "readonly",
        HTMLSelectElement: "readonly",
        HTMLImageElement: "readonly",
        DataTransfer: "readonly",
        DragEvent: "readonly",
        Node: "readonly",
        NodeList: "readonly",
        Event: "readonly",
        MouseEvent: "readonly",
        KeyboardEvent: "readonly",
        XPathResult: "readonly",
        setTimeout: "readonly",
        clearTimeout: "readonly",
        Array: "readonly",
        Promise: "readonly",
        Date: "readonly",
        JSON: "readonly",
        Error: "readonly",
        String: "readonly",
        CSS: "readonly",
        structuredClone: "readonly",
        NodeFilter: "readonly",
        SubmitEvent: "readonly",
        MutationObserver: "readonly",
      },
    },
    plugins: {
      jsdoc,
    },
    rules: {
      // Override defaults
      "no-unused-vars": ["error", {
        argsIgnorePattern: "^_",
        varsIgnorePattern: "^_",
      }],

      // Best practices
      "curly": ["error", "all"],
      "eqeqeq": ["error", "always"],
      "no-eval": "error",
      "no-implied-eval": "error",
      "no-return-await": "error",
      "no-throw-literal": "error",
      "prefer-const": "error",
      "no-var": "error",

      // Code style
      "indent": ["error", 2, { SwitchCase: 1 }],
      "quotes": ["error", "single", { avoidEscape: true }],
      "semi": ["error", "always"],
      "comma-dangle": ["error", "never"],
      "array-bracket-spacing": ["error", "never"],
      "object-curly-spacing": ["error", "always"],
      "space-before-blocks": "error",
      "keyword-spacing": "error",
      "space-infix-ops": "error",
      "no-trailing-spaces": "error",
      "eol-last": "error",
      "no-multiple-empty-lines": ["error", { max: 2, maxEOF: 1 }],

      // JSDoc rules
      "jsdoc/check-alignment": "warn",
      "jsdoc/check-param-names": "warn",
      "jsdoc/check-tag-names": "warn",
      "jsdoc/check-types": "warn",
      "jsdoc/require-jsdoc": ["warn", {
        require: {
          FunctionDeclaration: true,
          MethodDefinition: true,
          ClassDeclaration: true,
        },
      }],
      "jsdoc/require-param": "warn",
      "jsdoc/require-param-type": "warn",
      "jsdoc/require-returns": "warn",
      "jsdoc/require-returns-type": "warn",

      // Disable some unicorn rules that don't make sense for browser extensions
      "unicorn/prefer-module": "off", // Browser extensions use different module system
      "unicorn/prefer-top-level-await": "off", // Not supported in extensions
      "unicorn/prefer-node-protocol": "off", // Not applicable to browser
      "unicorn/filename-case": "off", // We already have our naming convention
      "unicorn/prevent-abbreviations": "off", // Too opinionated
      "unicorn/prefer-global-this": "off", // window is needed for MouseEvent view property
    },
  },
];
