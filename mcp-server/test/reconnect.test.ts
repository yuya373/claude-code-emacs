import { ReconnectManager } from '../src/reconnect';

describe('ReconnectManager', () => {
  beforeEach(() => {
    jest.useFakeTimers();
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  function make(overrides: Record<string, unknown> = {}) {
    const register = jest.fn<Promise<void>, []>().mockResolvedValue(undefined);
    const isConnected = jest.fn<boolean, []>().mockReturnValue(false);
    const log = jest.fn();
    const manager = new ReconnectManager({
      register,
      isConnected,
      log,
      initialDelayMs: 1000,
      maxDelayMs: 8000,
      ...overrides
    });
    return { manager, register, isConnected, log };
  }

  it('retries register with exponential backoff capped at maxDelayMs', async () => {
    const { manager, register } = make();
    manager.start();

    // No attempt before the initial delay elapses
    expect(register).not.toHaveBeenCalled();

    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(1);

    await jest.advanceTimersByTimeAsync(2000);
    expect(register).toHaveBeenCalledTimes(2);

    await jest.advanceTimersByTimeAsync(4000);
    expect(register).toHaveBeenCalledTimes(3);

    await jest.advanceTimersByTimeAsync(8000);
    expect(register).toHaveBeenCalledTimes(4);

    // Delay is capped at maxDelayMs from here on
    await jest.advanceTimersByTimeAsync(8000);
    expect(register).toHaveBeenCalledTimes(5);
  });

  it('stops retrying once the connection is restored', async () => {
    const { manager, register, isConnected } = make();
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(1);

    // Emacs reconnected before the next tick
    isConnected.mockReturnValue(true);
    await jest.advanceTimersByTimeAsync(2000);
    // The tick sees the restored connection and does not re-register
    expect(register).toHaveBeenCalledTimes(1);
    expect(manager.isRunning()).toBe(false);

    // No further attempts are scheduled
    await jest.advanceTimersByTimeAsync(60000);
    expect(register).toHaveBeenCalledTimes(1);
  });

  it('stop cancels pending attempts', async () => {
    const { manager, register } = make();
    manager.start();
    manager.stop();

    await jest.advanceTimersByTimeAsync(60000);
    expect(register).not.toHaveBeenCalled();
    expect(manager.isRunning()).toBe(false);
  });

  it('keeps retrying when register fails', async () => {
    const { manager, register } = make();
    register.mockRejectedValue(new Error('emacsclient: connection refused'));
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(1);

    // The failure must not break the loop
    await jest.advanceTimersByTimeAsync(2000);
    expect(register).toHaveBeenCalledTimes(2);
  });

  it('does not double-schedule when started twice', async () => {
    const { manager, register } = make();
    manager.start();
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(1);
  });

  it('resets the backoff when restarted', async () => {
    const { manager, register } = make();
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    await jest.advanceTimersByTimeAsync(2000);
    expect(register).toHaveBeenCalledTimes(2);

    manager.stop();
    manager.start();

    // First attempt after restart happens at the initial delay again
    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(3);
  });

  it('does not fork the retry loop when stopped and restarted during an in-flight register', async () => {
    let resolveFirst!: () => void;
    const first = new Promise<void>((resolve) => {
      resolveFirst = resolve;
    });
    const register = jest.fn<Promise<void>, []>()
      .mockImplementationOnce(() => first)
      .mockResolvedValue(undefined);
    const isConnected = jest.fn<boolean, []>().mockReturnValue(false);
    const manager = new ReconnectManager({
      register,
      isConnected,
      initialDelayMs: 1000,
      maxDelayMs: 8000
    });

    manager.start();
    await jest.advanceTimersByTimeAsync(1000);
    // First register is in flight (emacsclient can block for seconds)
    expect(register).toHaveBeenCalledTimes(1);

    // Connection blips while the register is still awaited:
    // 'connect' -> stop(), then 'disconnect' -> start()
    manager.stop();
    manager.start();

    // The in-flight tick resumes; it must NOT schedule a second chain
    resolveFirst();
    await jest.advanceTimersByTimeAsync(0);

    // Only the restarted chain runs: one attempt at the initial delay...
    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(2);

    // ...and nothing extra from the stale chain in between
    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(2);

    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(3);
  });

  it('gives up after maxAttempts and stops spawning emacsclient', async () => {
    const { manager, register, log } = make({ maxAttempts: 3 });
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    await jest.advanceTimersByTimeAsync(2000);
    await jest.advanceTimersByTimeAsync(4000);
    expect(register).toHaveBeenCalledTimes(3);
    expect(manager.isRunning()).toBe(false);

    // No further attempts, ever
    await jest.advanceTimersByTimeAsync(600000);
    expect(register).toHaveBeenCalledTimes(3);
    expect(log).toHaveBeenCalledWith(expect.stringContaining('Giving up'));
  });

  it('restart after giving up begins a fresh attempt budget', async () => {
    const { manager, register } = make({ maxAttempts: 2 });
    manager.start();

    await jest.advanceTimersByTimeAsync(1000);
    await jest.advanceTimersByTimeAsync(2000);
    expect(register).toHaveBeenCalledTimes(2);
    expect(manager.isRunning()).toBe(false);

    // A later disconnect event may start the manager again
    manager.start();
    await jest.advanceTimersByTimeAsync(1000);
    expect(register).toHaveBeenCalledTimes(3);
  });
});
