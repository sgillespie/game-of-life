{ glfw3, glm, libX11, libXxf86vm, libXrandr, libXi, safe, shaderc,
  spirv-tools, vulkan-headers, vulkan-loader, vulkan-tools, vulkan-tools-lunarg,
  vulkan-validation-layers,
  
  mkDerivation, base, bytestring, lib, linear, resourcet, sdl2,
  text, transformers, unliftio-core, vector, vulkan, vulkan-utils,
  VulkanMemoryAllocator
}:
mkDerivation {
  pname = "gillespies-game-of-life";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableSystemDepends = [
    glfw3
    glm
    libX11
    libXxf86vm
    libXrandr
    libXi
    safe
    shaderc
    spirv-tools
    vulkan-headers
    vulkan-loader
    vulkan-tools
    vulkan-tools-lunarg
    vulkan-validation-layers
  ];
  executableHaskellDepends = [
    base bytestring linear resourcet sdl2 text transformers
    unliftio-core vector vulkan vulkan-utils VulkanMemoryAllocator
  ];
  homepage = "https://github.com/sgillespie/game-of-life";
  description = "Submission for GDFG's Monthly Game Jam #8";
  license = lib.licenses.bsd3;
}
