# GermanOnly
Demonstration of how to write an application which only uses German strings

- Don't activate i18n in the project options.
- Don't use Default/LCLTranslator.
- Write your application with hard-coded German strings (or with German resourcestrings)
- If you use other packages, copy the German po files into the directory of the executable. This step is usually needed for the LCL strings, otherwise some buttons or dialogs will still be labeled in English. The German po file is `lcl/languages/lclstrconsts.de.po` (in the directory of the Lazarus installation).
- In the initialization part of the main form (or in the project unit) execute the translation manually by calling `TranslateUnitResourceStrings` in the translations unit for these po files.

You must distribute the copied po files along with your binary. See demo project in folder "normal_version".

If you don't want that you can add the needed po files to the application as a resource:

- In the project options, go to "Resource" > "Add" and select the copied po files to be added. Make sure that the added files have resource type `RT_RCDATA`.
- In the initialization part of the main form (or in the project unit) you must extract the po files from the resource and store them in a temporary folder before they can be translated by `TranslateUnitResourceStrings` - see demo project in folder "resource_version".
- In this case the po files need not be distributed along with the executable. They are only needed for compilation.
