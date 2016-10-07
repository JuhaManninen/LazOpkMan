{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   Constants, resource strings for the online package manager.
}
unit opkman_const;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cRemoteJSONFile = 'packagelist.json';
  cLocalRepository =  'onlinepackagemanager';
  cLocalRepositoryPackages = 'packages';
  cLocalRepositoryArchive = 'archive';
  cLocalRepositoryConfig = 'config';
  cLocalRepositoryConfigFile = 'options.xml';

resourcestring
  //package manager
  rsLazarusPackageManager = 'Online Package Manager';

  //main form
  rsMainFrmCaption = 'packages found';
  rsMainFrmVSTHeaderColumnPackageName = 'Packagename';
  rsMainFrmVSTHeaderColumnData = 'Status/Data';
  rsMainFrmVSTTextPackageStatus = 'Package status';
  rsMainFrmVSTTextVersion = 'Version';
  rsMainFrmVSTTextDescription = 'Description';
  rsMainFrmVSTTextAuthor = 'Author';
  rsMainFrmVSTTextCompatible = 'Compatible with';
  rsMainFrmVSTTextPackagetype = 'Package type';
  rsMainFrmVSTTextDependecies = 'Dependencies';
  rsMainFrmVSTTextLicense = 'License';
  rsMainFrmVSTTextMiscellaneous = 'Miscellaneous';
  rsMainFrmVSTTextRepositoryFilename = 'Repository filename';
  rsMainFrmVSTTextRepositoryFileSize = 'Repository filesize';
  rsMainFrmVSTTextRepositoryFileHash = 'Repository filehash';
  rsMainFrmVSTTextRepositoryFileDate = 'Available since';
  rsMainFrmVSTTextPackageType0 = 'Designtime and runtime';
  rsMainFrmVSTTextPackageType1 = 'Designtime';
  rsMainFrmVSTTextPackageType2 = 'Runtime';
  rsMainFrmVSTTextPackageType3 = 'Runtime only, cannot be installed in IDE';
  rsMainFrmcbAllCaption = 'All/None';
  rsMainFrmcbAllHint = 'Check/Uncheck packages';
  rsMainFrmcbFilterByHint = 'Filter package list by:';
  rsMainFrmedFilterHint = 'Type filter text';
  rsMainFrmspClearHint = 'Clear filter text';
  rsMainFrmspRefreshHint = 'Refesh package list';
  rsMainFrmspExpandHint = 'Expand package tree';
  rsMainFrmspCollapseHint = 'Collapse package tree';
  rsMainFrmbOptionsCaption = 'Options';
  rsMainFrmOptionsHint = 'Open options dialog';
  rsMainFrmbInstallCaption = 'Install';
  rsMainFrmbInstallHint = 'Install/Download';
  rsMainFrmPackageState0 = 'Available for download/install';
  rsMainFrmPackageState1 = 'Downloaded, ready to extract/install';
  rsMainFrmPackageState2 = 'Downloaded, extracted, ready to install';
  rsMainFrmPackageState3 = 'Already installed';
  rsMainFrmmiJSONShow =  'Show JSON';
  rsMainFrmmiJSONHide = 'Hide JSON';
  rsMainFrmmiInstall = 'Install';
  rsMainFrmmiDownloadUnZipp = 'Download to..';
  rsMainFrmPackagenameAlreadyExists = 'A package with the same name already exists!';
  rsMainFrmFilenameAlreadyExists = 'A package with the same zip file already exists!';
  rsMainFrmPackageAlreadyInstalled = 'The following package(s) are alrady installed. Continue with install?';
  rsMainFrmPackageAlreadyDownloaded = 'The following pakcage(s) already exists in the target folder. Continue?';

  //progress form
  rsProgressFrmCaption0 = 'Downloading packages';
  rsProgressFrmCaption1 = 'Extracting packages';
  rsProgressFrmCaption2 = 'Installing packages';
  rsProgressFrmCaption3 = '. Please wait...';
  rsProgressFrmlbPackageCaption = 'Package:';
  rsProgressFrmlbSpeedCaption = 'Speed:';
  rsProgressFrmlbSpeedCalcCaption = 'Estimating. Please wait...';
  rsProgressFrmlbEllapsedCaption = 'Ellapsed:';
  rsProgressFrmlbRemainingCaption = 'Remaining:';
  rsProgressFrmlbReceivedCaption0 = 'Received:';
  rsProgressFrmlbReceivedCaption1 = 'Unzipped:';
  rsProgressFrmlbReceivedTotalCaption0 = 'Received(total):';
  rsProgressFrmlbReceivedTotalCaption1 = 'Unzipped(total):';
  rsProgressFrmcbExtractOpenCaption0 = 'Extract after download';
  rsProgressFrmcbExtractOpenCaption1 = 'Open containing folder';
  rsProgressFrmCancelDownload = 'Cancel download?';
  rsProgressFrmCancelZip = 'Cancel zip?';
  rsProgressFrmError0 = 'Cannot download package:';
  rsProgressFrmError1 = 'Error message:';
  rsProgressFrmError2 = 'Cannot extract package:';
  rsProgressFrmError3 = 'Cannot install package:';
  rsProgressFrmConfirm0 = 'Continue with next one?';
  rsProgrssFrmInfo0 = 'Installing package:';
  rsProgrssFrmInfo1 = 'Success.';

  //options form
  rsOptionsFrmCaption = 'Options';
  rsOptionedRemoteRepository = 'Please enter the remote repository address!';

  //package list form
  rsPackageListFrmCaption0 = 'Installed package list';
  rsPackageListFrmCaption1 = 'Downloaded package list';
  rsPackageListFrmbYes = 'Yes';
  rsPackageListFrmbNo = 'No';

  //messages
  rsMessageNoPackage = 'No packages to show.';
  rsMessageDownload = 'Downloading package list. Please wait...';
  rsMessageNoRepository0 = 'Remote package repository not configured.';
  rsMessageNoRepository1 = 'Do you wish to configure it now?';
  rsMessageError0 = 'Cannot download package list. Error message:';
  rsMessageError1 = 'Invalid JSON file.';
  rsMessageError2 = 'Remote server unreachable.';
  rsNoPackageToDownload = 'Please select(check) one or more packages!';
  rsRepositoryCleanup0 = 'This will delete all non-installed packages from local repository. Continue?';
  rsRepositoryCleanup1 = 'packages deleted!';
  rsPackageDependency0 = 'dependends on package:';
  rsPackageDependency1 = 'Resolve dependecy?';
  rsPackageDependency2 = 'Not resolving dependencies might lead to install failure!';
  rsPackageDependency3 = 'Package dependency successfully resolved!';
  rsPackageDependency4 = 'is in';
  rsPackageDependency5 = 'dependency list. Unchecking it might lead to installing failure. Do you wish to continue?';

implementation

end.









