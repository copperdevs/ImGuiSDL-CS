using CopperDevs.Windowing;
using CopperDevs.Windowing.Data;
using CopperDevs.Windowing.SDL3;
using CopperDevs.Windowing.SDL3.Data;
using ImGuiNET;
using SDL;

namespace ImGuiSDL;

public static unsafe class Program
{
    private static SDL3Window window = null!;
    private static SDLRenderer renderer = null!;

    private static ImGuiRenderer Renderer = null!;

    private static SDL_GPUDevice* Device = null!;

    public static void Main()
    {
        var options = SDL3WindowOptions.Default with
        {
            Title = "ImGuiSDL-CS",
            Metadata = new AppMetadata
            {
                Name = "ImGuiSDL-CS",
                Version = "1.0.0",
                Type = AppMetadata.AppType.Application
            }
        };

        window = Window.Create<SDL3Window>(options);
        renderer = window.GetRenderer();

        window.OnLoad += OnLoad;
        window.OnUpdate += OnUpdate;
        window.OnRender += OnRender;
        window.OnClose += OnClose;

        window.Run();

        window.Dispose();
    }

    private static void OnLoad()
    {
        Device = SDLAPI.CreateGpuDevice(
            SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_DXIL | SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_MSL | SDL_GPUShaderFormat.SDL_GPU_SHADERFORMAT_SPIRV,
            true,
            (byte*)null);

        if (Device == null)
            throw new Exception($"{nameof(SDLAPI.CreateGpuDevice)} Failed: {SDLAPI.GetError()}");

        if (!SDLAPI.ClaimWindowForGpuDevice(Device, window.GetNativeWindow()))
            throw new Exception($"{nameof(SDLAPI.ClaimWindowForGpuDevice)} Failed: {SDLAPI.GetError()}");

        var context = ImGui.CreateContext();
        ImGui.SetCurrentContext(context);

        Renderer = new ImGuiRenderer(Device, window.GetNativeWindow(), context);
    }

    private static void OnUpdate()
    {
        if (ImGui.GetIO().WantTextInput && !SDLAPI.TextInputActive(window.GetNativeWindow()))
            SDLAPI.StartTextInput(window.GetNativeWindow());
        
        else if (!ImGui.GetIO().WantTextInput && SDLAPI.TextInputActive(window.GetNativeWindow()))
            SDLAPI.StopTextInput(window.GetNativeWindow());


        ImGui.GetIO().DeltaTime = Time.DeltaTimeAsFloat;
    }

    private static void OnRender()
    {
        Renderer.NewFrame();

        ImGui.NewFrame();
        if (ImGui.Begin("Hello World"))
        {
            ImGui.Text("This is ImGui x SDL x C#");
        }

        ImGui.End();
        ImGui.ShowDemoWindow();
        ImGui.EndFrame();

        Renderer.Render(new SDL_FColor { r = 0.1f, g = 0.05f, b = 0.08f, a = 1.0f });
    }

    private static void OnClose()
    {
        ImGui.DestroyContext(Renderer.Context);
        SDLAPI.ReleaseWindowFromGpuDevice(Device, window.GetNativeWindow());
        SDLAPI.DestroyGpuDevice(Device);
    }
}