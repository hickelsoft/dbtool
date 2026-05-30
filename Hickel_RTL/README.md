# Hickel_RTL

Hickel_RTL contains various runtime methods shared across all HickelSOFT products.

As part of the Open Source release of DBTool, both Hickel_RTL and Hickel_VCL were also released as Open Source under the terms of the Apache License 2.0.

However, the following units are excluded from the Open Source release due to their proprietary and confidential nature, and have therefore been redacted:

- HICKEL_RTL\Lib\Src\hl_HSK_Client.pas
- HICKEL_RTL\Lib\Src\hl_Lizenz.pas
- HICKEL_RTL\Lib\Src\HS_Mitteilung.pas

These proprietary HickelSOFT-specific integration units are replaced with stub implementations in community builds.

### hl_HSK_Client.pas
"HSK" refers to internal methods used to support various custom configurations and proprietary features of HickelSOFT products.
As these features are proprietary, they are not included in the Open Source release of DBTool. Therefore, the corresponding methods are replaced with dummy implementations.
While the official DBTool build includes a fully implemented version of Hickel_HSK*.dll, community builds and forks can operate normally using the dummy implementation without requiring the DLL.
(Regular users are unlikely to notice any missing functionality, as the unavailable features primarily affect HickelSOFT-specific use cases and integrations.)

### hl_Lizenz.pas
This unit provides licensing functionality for HickelSOFT products.
As the licensing mechanisms are proprietary and confidential, this unit is not included in the Open Source release of DBTool.
DBTool itself does not perform any license checks and operates fully without this unit.

### HS_Mitteilung.pas
This unit is responsible for communication between customer systems and the HickelSOFT server, regulated by the Data Processing Agreement.
The underlying communication mechanisms are proprietary and confidential, and are therefore not included in the Open Source release of DBTool.
Only a small number of HickelSOFT products use this functionality, and DBTool is not among them.
Please note that neither the official DBTool build nor community builds communicate with the HickelSOFT server, with the sole exception of the update check mechanism, which is not part of HS_Mitteilung.pas.