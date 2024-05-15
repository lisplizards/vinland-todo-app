/*
 * Copyright (c) 2024 John Newton
 * SPDX-License-Identifier: Apache-2.0
 */

import { Controller } from "@hotwired/stimulus";

export default class extends Controller {
  static targets = ["spinner", "form"];
  static classes = ["hidden"];

  showSpinner() {
    this.formTarget.classList.add(this.hiddenClass);
    this.spinnerTarget.classList.remove(this.hiddenClass);
  }

  cancelSpinner() {
    this.spinnerTarget.classList.add(this.hiddenClass);
    this.formTarget.classList.remove(this.hiddenClass);
  }
}
