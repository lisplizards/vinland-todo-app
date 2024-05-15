/*
 * Copyright (c) 2024 John Newton
 * SPDX-License-Identifier: Apache-2.0
 */

import * as Turbo from "@hotwired/turbo";
import { Application } from "@hotwired/stimulus";
import SessionSpinnerController from "./controllers/session-spinner-controller.js";
import TodoListController from "./controllers/todo-list-controller.js";

const application = Application.start();

application.register("session-spinner", SessionSpinnerController);
application.register("todo-list", TodoListController);
