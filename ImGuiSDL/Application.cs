using System.Numerics;
using ImGuiNET;
using static SDL3.SDL;

namespace ImGuiSDL;

internal class Application : IDisposable
{
	/// <summary>
	/// The SDL Window
	/// </summary>
	public readonly nint Window;

	/// <summary>
	/// The SDL GPU Device
	/// </summary>
	public readonly nint Device;

	/// <summary>
	/// The ImGui Renderer Implementation
	/// </summary>
	public readonly ImGuiRenderer Renderer;

	/// <summary>
	/// If the Application is Running
	/// </summary>
	public bool Running { get; private set; }

	public Application(string name, int width, int height)
	{
		// launch SDL
		if (!SDL_Init(SDL_InitFlags.SDL_INIT_VIDEO))
			throw new Exception($"{nameof(SDL_Init)} Failed: {SDL_GetError()}");

		// create graphics device
		Device = SDL_CreateGPUDevice(
			SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_DXIL | 
			SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_MSL | 
			SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_SPIRV,
			debug_mode: true, null!);
		if (Device == nint.Zero)
			throw new Exception($"{nameof(SDL_CreateGPUDevice)} Failed: {SDL_GetError()}");

		// create window
		var windowFlags = SDL_WindowFlags.SDL_WINDOW_RESIZABLE;
		Window = SDL_CreateWindow(name, width, height, windowFlags);
		if (Window == nint.Zero)
			throw new Exception($"{nameof(SDL_CreateWindow)} Failed: {SDL_GetError()}");

		// claim SDL GPU Window
		if (!SDL_ClaimWindowForGPUDevice(Device, Window))
			throw new Exception($"{nameof(SDL_ClaimWindowForGPUDevice)} Failed: {SDL_GetError()}");

		// setup frames in flight
		SDL_SetGPUAllowedFramesInFlight(Device, 3);

		// create imgui context
		var context = ImGui.CreateContext();
		ImGui.SetCurrentContext(context);

		// create imgui SDL_GPU renderer
		Renderer = new ImGuiRenderer(Device, Window, context);

		// set renderer scale
		var display = SDL_GetDisplayForWindow(Window);
		Renderer.Scale = SDL_GetDisplayContentScale(display);

		// setup imgui properties
		{
			SDL_GetWindowSizeInPixels(Window, out width, out height);
			ImGui.GetIO().DisplaySize = new Vector2(width, height);
		}
	}

	~Application() => Dispose();

    public void Dispose()
    {
		GC.SuppressFinalize(this);
		ImGui.DestroyContext(Renderer.Context);
		Running = false;
		Renderer.Dispose();
		SDL_ReleaseWindowFromGPUDevice(Device, Window);
		SDL_DestroyWindow(Window);
		SDL_DestroyGPUDevice(Device);
		SDL_Quit();
    }

    public void Run()
	{
		Running = true;

		while (Running)
		{
			PollEvents();
			Update();
			Render();
		}
	}

	public void PollEvents()
	{
		while (SDL_PollEvent(out var ev))
		{
			Renderer.ProcessEvent(ev);

			switch ((SDL_EventType)ev.type)
			{
				case SDL_EventType.SDL_EVENT_WINDOW_CLOSE_REQUESTED:
				case SDL_EventType.SDL_EVENT_QUIT:
					Running = false;
					break;
			}
		}
	}

	public void Update()
	{
		Renderer.NewFrame();

		if (ImGui.Begin("Hello World"))
		{
			ImGui.Text("This is ImGui x SDL x C#");
		}
		ImGui.End();

		ImGui.ShowDemoWindow();
		
		Renderer.EndFrame();
	}

	public void Render()
	{
		var clear = new SDL_FColor() { r = 0.1f, g = 0.05f, b = 0.08f, a = 1.0f };
		Renderer.Render(clear);
	}
}