// Re-registration retry loop for the Emacs connection.
//
// When Emacs restarts it loses every registered MCP port, while this
// server process (hosted by the claude daemon for background sessions
// and agents) keeps running. Registration normally happens only once at
// startup, so the connection would stay orphaned forever. This manager
// retries the emacsclient registration with exponential backoff until
// Emacs connects back, and goes quiet as soon as it does.

export interface ReconnectOptions {
  /** Attempt to re-register this instance with Emacs (via emacsclient). */
  register: () => Promise<void>;
  /** Whether the Emacs WebSocket connection is currently established. */
  isConnected: () => boolean;
  log?: (message: string) => void;
  /** Delay before the first attempt. Defaults to 5000ms. */
  initialDelayMs?: number;
  /** Upper bound for the backoff delay. Defaults to 60000ms. */
  maxDelayMs?: number;
  /**
   * Give up after this many attempts so an abandoned session does not
   * spawn emacsclient forever. Defaults to 120 (roughly two hours with
   * the default delays). A later start() call begins a fresh budget.
   */
  maxAttempts?: number;
}

export class ReconnectManager {
  private timer?: NodeJS.Timeout;
  private attempt = 0;
  private running = false;
  // Incremented on every start()/stop() so a tick resumed from an
  // awaited register can tell that it belongs to a stale retry chain
  // and must not schedule a competing timer.
  private generation = 0;

  constructor(private readonly options: ReconnectOptions) {}

  start(): void {
    if (this.running) {
      return;
    }
    this.running = true;
    this.attempt = 0;
    this.generation++;
    this.schedule(this.generation);
  }

  stop(): void {
    this.running = false;
    this.generation++;
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = undefined;
    }
  }

  isRunning(): boolean {
    return this.running;
  }

  private delayFor(attempt: number): number {
    const initial = this.options.initialDelayMs ?? 5000;
    const max = this.options.maxDelayMs ?? 60000;
    return Math.min(initial * 2 ** attempt, max);
  }

  private schedule(generation: number): void {
    if (!this.running || generation !== this.generation) {
      return;
    }
    this.timer = setTimeout(() => {
      void this.tick(generation);
    }, this.delayFor(this.attempt));
    // Never keep the process alive just for a retry
    this.timer.unref?.();
  }

  private async tick(generation: number): Promise<void> {
    if (!this.running || generation !== this.generation) {
      return;
    }
    if (this.options.isConnected()) {
      this.options.log?.('Emacs connection restored; stopping re-register retries');
      this.stop();
      return;
    }
    this.attempt++;
    this.options.log?.(`Re-register attempt ${this.attempt} (Emacs not connected)`);
    try {
      await this.options.register();
    } catch (error) {
      this.options.log?.(`Re-register attempt ${this.attempt} failed: ${error}`);
    }
    const maxAttempts = this.options.maxAttempts ?? 120;
    if (this.attempt >= maxAttempts) {
      this.options.log?.(
        `Giving up re-registration after ${this.attempt} attempts; ` +
          'restart Emacs and re-register manually if needed'
      );
      this.stop();
      return;
    }
    this.schedule(generation);
  }
}
