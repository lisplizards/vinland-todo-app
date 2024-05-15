/*
 * Copyright (c) 2024 John Newton
 * SPDX-License-Identifier: Apache-2.0
 */

import * as Turbo from "@hotwired/turbo";
import { Controller } from "@hotwired/stimulus";
import { serialize } from "@shoelace-style/form";

export default class extends Controller {
  static targets = [];

  connect() {
    this.element.addEventListener("sl-change", this.#toggleTaskState);
  }

  #toggleTaskState = (event) => {
    if (event.target.tagName !== "SL-CHECKBOX") {
      return;
    }
    const input = event.target;
    input.disabled = true;
    const taskEl = input.closest("li");
    const form = input.closest("form");
    const data = serialize(form);
    // Coerce checkbox value to boolean
    data.completed = input.checked;

    fetch(form.action, {
      method: "PATCH",
      body: JSON.stringify(data),
      headers: {
        "Content-Type": "application/json; charset=utf-8",
        "Accept": "text/vnd.turbo-stream.html"
      }
    }).then((res) => res.text())
      .then((html) => Turbo.renderStreamMessage(html))
      .catch((err) => {
        console.error("error", err);
        input.disabled = false;
      });
  }
}
